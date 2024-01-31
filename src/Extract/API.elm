module Extract.API exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Exp exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature as Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TAnn exposing (TypeAnnotation)
import Extract.Internal.TypeDef as TypeDef
import Extract.Internal.Util exposing (flip)
import Json.Encode as Encode
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "Extract.API" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.withDataExtractor reportAPI
        |> Rule.fromProjectRuleSchema


reportAPI : ProjectContext -> Encode.Value
reportAPI { entryPoints } =
    entryPoints
        |> Dict.toList
        |> List.map (Tuple.mapBoth (String.join ".") encodeEntryPoint)
        |> Encode.object


encodeEntryPoint : EntryPoint -> Encode.Value
encodeEntryPoint entryPoint =
    Encode.object
        [ ( "ports", encodePorts entryPoint.ports )
        , ( "flags", encodeType entryPoint.flags )
        ]


encodeType : Maybe TypeDef.ConcreteTypeDef -> Encode.Value
encodeType =
    Maybe.andThen TypeDef.toCodable
        >> Maybe.map TypeDef.encode
        >> Maybe.withDefault (Encode.string "unknown")


encodePorts : Dict ( ModuleName, String ) PortInfo -> Encode.Value
encodePorts =
    Dict.toList
        >> List.map (Tuple.mapBoth Tuple.second encodePortInfo)
        >> Encode.object


encodePortInfo : PortInfo -> Encode.Value
encodePortInfo portInfo =
    Encode.object
        [ ( "direction"
          , Encode.string <|
                case portInfo.direction of
                    CmdPort ->
                        "cmd"

                    SubPort ->
                        "sub"
          )
        , ( "type", encodeType portInfo.type_ )
        ]


type alias EntryPoint =
    { ports : Dict ( ModuleName, String ) PortInfo
    , flags : Maybe TypeDef.ConcreteTypeDef
    }


type alias Port =
    { name : ( ModuleName, String )
    , info : PortInfo
    }


type alias PortInfo =
    { direction : PortDirection
    , type_ : Maybe TypeDef.ConcreteTypeDef
    }


type PortDirection
    = CmdPort
    | SubPort


type alias ProjectContext =
    { entryPoints : Dict ModuleName EntryPoint
    , ports : Dict ( ModuleName, String ) PortInfo
    , portCallers : Dict ( ModuleName, String ) (Set ( ModuleName, String ))
    , typeDefs : Dict ( ModuleName, String ) TypeDef.TypeDef
    }


type alias Scope =
    { variables : Set String
    , calls : Set ( ModuleName, String )
    }


type alias ModuleContext =
    { projectContext : ProjectContext
    , typeDefs : Dict String TypeDef.TypeDef
    , callGraph : Dict String (Set ( ModuleName, String ))
    , lookup : ModuleNameLookupTable
    , entryPoint : Maybe Signature
    , moduleName : ModuleName
    , defsInScope : Dict ( ModuleName, String ) TypeDef.TypeDef
    , defsToProcess : Dict String ( List String, TypeAnnotation )
    , hasPorts : Bool
    , portsToProcess : List ( String, PortDirection, TypeAnnotation )

    -- These are "scoped" to a single top level declaration, and reset
    -- at the end of processing each declaration. They're really more of
    -- a "declaration context", but we don't have such a thing builtin
    -- so we just go with it.
    , variables : ( Set String, List (Set String) )
    , calls : Set ( ModuleName, String )
    }


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List never, ModuleContext )
moduleDefinitionVisitor (Node _ mod) context =
    ( [], { context | hasPorts = context.hasPorts || Module.isPortModule mod } )


declarationVisitor : Node Declaration -> ModuleContext -> ( List never, ModuleContext )
declarationVisitor decl context =
    case Node.value decl of
        Declaration.FunctionDeclaration { declaration, signature } ->
            recordMainIfAny signature context
                |> when context.hasPorts (recordCallGraph declaration)
                |> Tuple.pair []

        Declaration.AliasDeclaration { name, generics, typeAnnotation } ->
            ( []
            , { context
                | defsToProcess =
                    Dict.insert (Node.value name)
                        ( List.map Node.value generics, Node.value typeAnnotation )
                        context.defsToProcess
              }
            )

        Declaration.PortDeclaration { name, typeAnnotation } ->
            addPortForProcessing name typeAnnotation context

        Declaration.CustomTypeDeclaration { name } ->
            ( []
            , { context
                | typeDefs =
                    Dict.insert
                        (Node.value name)
                        (TypeDef.createCtor ( context.moduleName, Node.value name ))
                        context.typeDefs
              }
            )

        _ ->
            ( [], context )


when : Bool -> (a -> a) -> a -> a
when cond f =
    if cond then
        f

    else
        identity


recordCallGraph : Node Exp.FunctionImplementation -> ModuleContext -> ModuleContext
recordCallGraph impl_ context =
    let
        impl =
            Node.value impl_
    in
    -- We only go here if we know ports are somehow in scope - either because this
    -- is a port module or it transitively imports a port module.
    --
    -- Later on, we'll cull and collect this callgraph to figure out which functions
    -- end up calling which ports.
    --
    -- first up, let's add ourselves and our patters to the scope
    -- then, we visit the actual expression!
    context
        |> addVariableToScope (Node.value impl.name)
        |> addVariablesToScope (List.concatMap (Node.value >> namesFromPattern) impl.arguments)
        |> visitExpression impl.expression
        |> scopeToCallGraph (Node.value impl.name)


addVariableToScope : String -> ModuleContext -> ModuleContext
addVariableToScope v ctx =
    addVariablesToScope [ v ] ctx


addVariablesToScope : List String -> ModuleContext -> ModuleContext
addVariablesToScope vs ctx =
    { ctx | variables = Tuple.mapFirst (\vars -> List.foldl Set.insert vars vs) ctx.variables }


inScope : String -> ModuleContext -> Bool
inScope val ctx =
    Set.member val (Tuple.first ctx.variables) || List.any (Set.member val) (Tuple.second ctx.variables)


recordCall : ( ModuleName, String ) -> ModuleContext -> ModuleContext
recordCall call ctx =
    { ctx | calls = Set.insert call ctx.calls }


visitExpression : Node Expression -> ModuleContext -> ModuleContext
visitExpression theExpression ctx =
    -- so what we're trying to do here, is figure out
    -- the callgraph. the way we're doing that, is basically by recursing
    -- through expressions, recording all functions they call.
    -- Easy peasy, right!
    case Node.value theExpression of
        Exp.Application app ->
            List.foldl visitExpression ctx app

        Exp.OperatorApplication _ _ left right ->
            ctx
                |> visitExpression left
                |> visitExpression right

        Exp.FunctionOrValue _ val ->
            case ModuleNameLookupTable.moduleNameFor ctx.lookup theExpression of
                Just [] ->
                    if inScope val ctx then
                        ctx

                    else
                        recordCall ( ctx.moduleName, val ) ctx

                Just moduleName ->
                    recordCall ( moduleName, val ) ctx

                Nothing ->
                    ctx

        Exp.IfBlock cond pos neg ->
            ctx
                |> visitExpression cond
                |> visitExpression pos
                |> visitExpression neg

        Exp.TupledExpression exprs ->
            List.foldl visitExpression ctx exprs

        Exp.ParenthesizedExpression exp ->
            visitExpression exp ctx

        Exp.LetExpression { declarations, expression } ->
            List.foldl visitLetDeclaration ctx declarations
                |> visitExpression expression

        Exp.CaseExpression { expression, cases } ->
            ctx
                |> visitExpression expression
                |> flip (List.foldl visitCase) cases

        Exp.LambdaExpression lambda ->
            ctx
                |> pushScope
                |> addVariablesToScope (List.concatMap (Node.value >> namesFromPattern) lambda.args)
                |> visitExpression lambda.expression
                |> popScope

        Exp.RecordExpr recordSetters ->
            List.foldl
                (\(Node _ ( _, setter )) -> visitExpression setter)
                ctx
                recordSetters

        Exp.ListExpr children ->
            List.foldl visitExpression ctx children

        Exp.RecordUpdateExpression _ recordSetters ->
            List.foldl
                (\(Node _ ( _, setter )) -> visitExpression setter)
                ctx
                recordSetters

        Exp.Negation exp ->
            visitExpression exp ctx

        -- Other expression types aren't of interest
        _ ->
            ctx


visitCase : Exp.Case -> ModuleContext -> ModuleContext
visitCase ( pat, exp ) ctx =
    ctx
        |> pushScope
        |> addVariablesToScope (namesFromPattern (Node.value pat))
        |> visitExpression exp
        |> popScope


visitLetDeclaration : Node Exp.LetDeclaration -> ModuleContext -> ModuleContext
visitLetDeclaration (Node _ theDeclaration) ctx =
    case theDeclaration of
        Exp.LetFunction { declaration } ->
            let
                impl =
                    Node.value declaration
            in
            ctx
                |> addVariableToScope (Node.value impl.name)
                |> pushScope
                |> addVariablesToScope (List.concatMap (Node.value >> namesFromPattern) impl.arguments)
                |> visitExpression impl.expression
                |> popScope

        Exp.LetDestructuring pat exp ->
            ctx
                |> addVariablesToScope (namesFromPattern (Node.value pat))
                |> pushScope
                |> visitExpression exp
                |> popScope


pushScope : ModuleContext -> ModuleContext
pushScope ctx =
    { ctx | variables = ( Set.empty, Tuple.first ctx.variables :: Tuple.second ctx.variables ) }


popScope : ModuleContext -> ModuleContext
popScope ({ variables } as ctx) =
    case Tuple.second variables of
        [] ->
            { ctx | variables = ( Set.empty, [] ) }

        top :: rest ->
            { ctx | variables = ( top, rest ) }


scopeToCallGraph : String -> ModuleContext -> ModuleContext
scopeToCallGraph name ctx =
    -- do the callgraph dance!
    { ctx
        | variables = ( Set.empty, [] )
        , calls = Set.empty
        , callGraph = Dict.insert name ctx.calls ctx.callGraph
    }


namesFromPattern : Pattern -> List String
namesFromPattern thePattern =
    case thePattern of
        TuplePattern vars ->
            List.concatMap (Node.value >> namesFromPattern) vars

        RecordPattern fields ->
            List.map Node.value fields

        UnConsPattern left right ->
            namesFromPattern (Node.value left) ++ namesFromPattern (Node.value right)

        ListPattern pats ->
            List.concatMap (Node.value >> namesFromPattern) pats

        VarPattern var ->
            [ var ]

        NamedPattern _ pats ->
            List.concatMap (Node.value >> namesFromPattern) pats

        AsPattern pat asName ->
            Node.value asName :: namesFromPattern (Node.value pat)

        ParenthesizedPattern pat ->
            namesFromPattern (Node.value pat)

        _ ->
            []


recordMainIfAny : Maybe (Node Signature) -> ModuleContext -> ModuleContext
recordMainIfAny signature context =
    case signature of
        Just (Node _ sig) ->
            if Node.value sig.name == "main" then
                { context | entryPoint = Just sig }

            else
                context

        Nothing ->
            context


addPortForProcessing : Node String -> Node TypeAnnotation -> ModuleContext -> ( List never, ModuleContext )
addPortForProcessing name typeAnnotation context =
    case Node.value typeAnnotation of
        TAnn.FunctionTypeAnnotation argType direction ->
            let
                res =
                    case ( Node.value argType, Node.value direction ) of
                        ( argType_, TAnn.Typed (Node _ ( _, "Cmd" )) _ ) ->
                            Just ( argType_, CmdPort )

                        ( TAnn.FunctionTypeAnnotation (Node _ argType_) _, TAnn.Typed (Node _ ( _, "Sub" )) _ ) ->
                            Just ( argType_, SubPort )

                        _ ->
                            Nothing
            in
            case res of
                Just ( portType, portDirection_ ) ->
                    ( []
                    , { context
                        | portsToProcess =
                            ( Node.value name
                            , portDirection_
                            , portType
                            )
                                :: context.portsToProcess
                      }
                    )

                Nothing ->
                    -- Should not be possible, but hey, can't do anything about it
                    ( [], context )

        _ ->
            -- A port that isn't a function type? Sure!
            ( [], context )


initialProjectContext : ProjectContext
initialProjectContext =
    { entryPoints = Dict.empty
    , ports = Dict.empty
    , portCallers = Dict.empty
    , typeDefs = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookup moduleName projectContext ->
            { projectContext = projectContext
            , typeDefs = Dict.empty
            , callGraph = Dict.empty
            , lookup = lookup
            , entryPoint = Nothing
            , moduleName = moduleName
            , defsToProcess = Dict.empty
            , defsInScope = projectContext.typeDefs
            , hasPorts = not (Dict.isEmpty projectContext.ports)
            , portsToProcess = []
            , variables = ( Set.empty, [] )
            , calls = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            -- okay, this is our chance to collect data and do all the things!
            moduleContext
                |> processTypeDefs
                |> processPorts
                |> pruneCallGraph
                |> withEntryPoint
                |> withTypeDefs
                |> .projectContext
        )


withTypeDefs : ModuleContext -> ModuleContext
withTypeDefs ({ projectContext } as ctx) =
    { ctx
        | projectContext =
            { projectContext
                | typeDefs =
                    Dict.toList ctx.typeDefs
                        |> List.map (Tuple.mapFirst (Tuple.pair ctx.moduleName))
                        |> Dict.fromList
                        |> Dict.union projectContext.typeDefs
            }
    }


pruneCallGraph : ModuleContext -> ModuleContext
pruneCallGraph ctx =
    let
        ( hasInteresting, result ) =
            Dict.foldl
                (\fnName calls acc ->
                    Set.foldl
                        (\( modName_, calledFnName ) ( interest_, acc_ ) ->
                            let
                                modName =
                                    if modName_ == [] then
                                        ctx.moduleName

                                    else
                                        modName_

                                calledFn =
                                    ( modName, calledFnName )

                                indirectPortCalls =
                                    Dict.get calledFn ctx.projectContext.portCallers
                            in
                            case ( Dict.member calledFn ctx.projectContext.ports, indirectPortCalls ) of
                                ( False, Nothing ) ->
                                    ( interest_, acc_ )

                                ( True, Nothing ) ->
                                    ( True, registerPortCall fnName (Set.singleton calledFn) acc_ )

                                ( False, Just indirect ) ->
                                    ( True, registerPortCall fnName indirect acc_ )

                                ( True, Just indirect ) ->
                                    ( True, registerPortCall fnName (Set.insert calledFn indirect) acc_ )
                        )
                        acc
                        calls
                )
                ( False, ctx )
                ctx.callGraph
    in
    if hasInteresting then
        pruneCallGraph result

    else
        result


registerPortCall : String -> Set ( ModuleName, String ) -> ModuleContext -> ModuleContext
registerPortCall name calls ({ projectContext } as ctx) =
    { ctx
        | callGraph = Dict.remove name ctx.callGraph
        , projectContext =
            { projectContext
                | portCallers =
                    Dict.update ( ctx.moduleName, name )
                        (Maybe.withDefault Set.empty >> Set.union calls >> Just)
                        projectContext.portCallers
            }
    }


processTypeDefs : ModuleContext -> ModuleContext
processTypeDefs mod =
    { mod
        | typeDefs =
            Dict.union
                (TypeDef.resolveTypes
                    { lookup = mod.lookup
                    , defsInScope = mod.defsInScope
                    , localDefs = mod.typeDefs
                    }
                    mod.defsToProcess
                )
                mod.typeDefs
        , defsToProcess = Dict.empty
    }


processPorts : ModuleContext -> ModuleContext
processPorts ({ projectContext } as mod) =
    { mod
        | projectContext =
            { projectContext
                | ports =
                    List.foldr
                        (\( portName, portDirection, tyAnn ) acc ->
                            Dict.insert ( mod.moduleName, portName )
                                { direction = portDirection
                                , type_ =
                                    TypeDef.resolveTypeAnnotation
                                        { lookup = mod.lookup
                                        , defsInScope = mod.defsInScope
                                        , localDefs = mod.typeDefs
                                        }
                                        tyAnn
                                }
                                acc
                        )
                        projectContext.ports
                        mod.portsToProcess
            }
    }


extractFlagsTypeAnnotation : ModuleContext -> Signature -> Maybe TypeDef.ConcreteTypeDef
extractFlagsTypeAnnotation mod sig =
    case
        TypeDef.resolveTypeAnnotation
            { lookup = mod.lookup
            , defsInScope = mod.defsInScope
            , localDefs = mod.typeDefs
            }
            (Node.value sig.typeAnnotation)
    of
        Just (TypeDef.TCustom ( [ "Platform" ], "Program" ) [ flags, _, _ ]) ->
            Just flags

        _ ->
            Nothing


actualType : TAnn.TypeAnnotation -> Maybe ( ModuleName, String )
actualType annotation =
    case annotation of
        TAnn.Typed (Node _ it) _ ->
            Just it

        _ ->
            Nothing


withEntryPoint : ModuleContext -> ModuleContext
withEntryPoint ({ projectContext } as mod) =
    case mod.entryPoint of
        Nothing ->
            mod

        Just entryPoint ->
            let
                ports =
                    Dict.get ( mod.moduleName, "main" ) projectContext.portCallers
                        |> Maybe.withDefault Set.empty
                        |> Set.toList
                        |> List.filterMap
                            (\name ->
                                Dict.get name projectContext.ports
                                    |> Maybe.map (Tuple.pair name)
                            )
                        |> Dict.fromList

                flags =
                    extractFlagsTypeAnnotation mod entryPoint
            in
            { mod
                | projectContext =
                    { projectContext
                        | entryPoints =
                            Dict.insert mod.moduleName
                                { ports = ports, flags = flags }
                                projectContext.entryPoints
                    }
            }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    { entryPoints = Dict.union new.entryPoints previous.entryPoints
    , ports = Dict.union new.ports previous.ports
    , portCallers = Dict.union new.portCallers previous.portCallers
    , typeDefs = Dict.union new.typeDefs previous.typeDefs
    }


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        _ ->
            ( [], context )
