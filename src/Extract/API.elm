module Extract.API exposing (rule, WithFlags(..))

{-|

@docs rule, WithFlags

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Exp exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature as Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TAnn exposing (TypeAnnotation)
import Extract.Internal.TypeDef as TypeDef
import Extract.Internal.Util exposing (flip, orElse)
import Json.Encode as Encode
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Whether information about flags should be included in the extract.

If this information isn't used, it can be nice to exclude it, so there's less need to
worry about running into pipe-buffer size limitations.

-}
type WithFlags
    = WithFlags
    | WithoutFlags


{-| Discover all entrypoints, record the ports and flags they use, and collect their types.

To actually get to the information, `elm-review` must be used with `--report=json --extract`.

-}
rule : WithFlags -> Rule
rule withFlags =
    Rule.newProjectRuleSchema "Extract.API" (initialProjectContext withFlags)
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
reportAPI { ports, entryPoints, withFlags } =
    Encode.object
        [ ( "ports", encodePorts ports )
        , ( "entryPoints"
          , entryPoints
                |> Dict.toList
                |> List.map (Tuple.mapBoth (String.join ".") (encodeEntryPoint withFlags))
                |> Encode.object
          )
        ]


encodeEntryPoint : WithFlags -> EntryPoint -> Encode.Value
encodeEntryPoint withFlags entryPoint =
    let
        ( cmds, subs ) =
            Dict.partition (\_ v -> v.direction == CmdPort) entryPoint.ports

        flagsData =
            if withFlags == WithFlags then
                [ ( "flags", encodeType entryPoint.flags ) ]

            else
                []
    in
    Encode.object
        (( "cmds", encodePortNames cmds )
            :: ( "subs", encodePortNames subs )
            :: flagsData
        )


encodeType : Maybe TypeDef.ConcreteTypeDef -> Encode.Value
encodeType =
    Maybe.andThen TypeDef.toCodable
        >> Maybe.map TypeDef.encode
        >> Maybe.withDefault (Encode.string "unknown")


encodePortNames : Dict ( ModuleName, String ) PortInfo -> Encode.Value
encodePortNames =
    Dict.keys >> Encode.list (Tuple.second >> Encode.string)


encodePorts : Dict ( ModuleName, String ) PortInfo -> Encode.Value
encodePorts =
    Dict.toList
        >> List.map (Tuple.mapBoth Tuple.second (.type_ >> encodeType))
        >> Encode.object


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
    { withFlags : WithFlags
    , entryPoints : Dict ModuleName EntryPoint
    , ports : Dict ( ModuleName, String ) PortInfo
    , portCallers : Dict ( ModuleName, String ) (Set ( ModuleName, String ))
    , typeDefs : Dict ( ModuleName, String ) TypeDef.TypeDef
    }


type alias Scope =
    { variables : Set String
    , calls : Set ( ModuleName, String )
    }


type ExposedThings
    = All
    | Selected (Set String)


type alias ModuleContext =
    { projectContext : ProjectContext
    , typeDefs : Dict String TypeDef.TypeDef
    , callGraph : Dict String (Set ( ModuleName, String ))
    , lookup : ModuleNameLookupTable
    , entryPoint : Maybe Signature
    , moduleName : ModuleName
    , defsToProcess : Dict String ( List String, TypeAnnotation )
    , hasPorts : Bool
    , portsToProcess : List ( String, PortDirection, TypeAnnotation )

    -- We want to only resolve types that are exposed, to save time and memory
    , exposedThings : ExposedThings

    -- These are "scoped" to a single top level declaration, and reset
    -- at the end of processing each declaration. They're really more of
    -- a "declaration context", but we don't have such a thing builtin
    -- so we just go with it.
    , variables : ( Set String, List (Set String) )
    , calls : Set ( ModuleName, String )
    }


isExposed : ModuleContext -> String -> Bool
isExposed ctx thing =
    case ctx.exposedThings of
        All ->
            True

        Selected things ->
            Set.member thing things


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List never, ModuleContext )
moduleDefinitionVisitor (Node _ mod) context =
    ( []
    , { context
        | hasPorts = context.hasPorts || Module.isPortModule mod
        , exposedThings = processExposing (Module.exposingList mod)
      }
    )


processExposing : Exposing -> ExposedThings
processExposing exp =
    case exp of
        Exposing.All _ ->
            All

        Exposing.Explicit items ->
            items
                |> List.filterMap
                    (\(Node _ exposedThing) ->
                        case exposedThing of
                            Exposing.InfixExpose _ ->
                                Nothing

                            Exposing.FunctionExpose name ->
                                Just name

                            Exposing.TypeOrAliasExpose name ->
                                Just name

                            Exposing.TypeExpose { name } ->
                                Just name
                    )
                |> Set.fromList
                |> Selected


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
    -- Later on, when we're done with the current module, we'll cull and collect
    -- this callgraph to figure out which functions end up calling which ports.
    --
    -- first up, let's add ourselves and our patters to the scope. Then, we visit
    -- the actual expression!
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
    -- so what we're trying to do here, is figure out the callgraph. the way
    -- we're doing that, is basically by recursing through expressions,
    -- recording all functions they call. Easy peasy, right!
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
                    -- if `val` refers to a variable passed through as an
                    -- argument of a function or is otherwise bound by a pattern
                    -- in a let declaration or a case-branch, we don't want to
                    -- record that as a "call": we're not calling a named
                    -- function
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

        Exp.RecordAccess exp _ ->
            visitExpression exp ctx

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


initialProjectContext : WithFlags -> ProjectContext
initialProjectContext withFlags =
    { withFlags = withFlags
    , entryPoints = Dict.empty
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
            , hasPorts = not (Dict.isEmpty projectContext.ports)
            , exposedThings = All
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
    findCallersOfKnownPortCallers ctx
        |> findIndirectLocalCallers ctx
        |> onlyExposedPortCalls ctx


findCallersOfKnownPortCallers : ModuleContext -> Dict String (Set ( ModuleName, String ))
findCallersOfKnownPortCallers ctx =
    let
        withRealModuleName ( modName, fnName ) =
            -- this means we're talking about a call to a function inthe current
            -- module, but we want to register and lookup calls fully qualified
            if modName == [] then
                ( ctx.moduleName, fnName )

            else
                ( modName, fnName )
    in
    Dict.foldl
        (\fnName calls acc ->
            Set.foldl
                (\calledFn acc_ ->
                    Dict.get (withRealModuleName calledFn) ctx.projectContext.portCallers
                        |> Maybe.map (registerCall fnName acc_)
                        |> Maybe.withDefault acc_
                )
                acc
                calls
        )
        Dict.empty
        ctx.callGraph


registerCall : String -> Dict String (Set ( ModuleName, String )) -> Set ( ModuleName, String ) -> Dict String (Set ( ModuleName, String ))
registerCall fnName acc calls =
    Dict.update fnName (Maybe.withDefault Set.empty >> Set.union calls >> Just) acc


findIndirectLocalCallers :
    ModuleContext
    -> Dict String (Set ( ModuleName, String ))
    -> Dict String (Set ( ModuleName, String ))
findIndirectLocalCallers ctx localPortCallers =
    let
        toLocalFn ( moduleName, fnName ) =
            if moduleName == ctx.moduleName then
                Just fnName

            else
                Nothing

        result =
            Dict.foldl
                (\fnName calls acc ->
                    Set.foldl
                        (\calledFn acc_ ->
                            toLocalFn calledFn
                                |> Maybe.andThen (flip Dict.get acc_)
                                |> Maybe.map (registerCall fnName acc_)
                                |> Maybe.withDefault acc_
                        )
                        acc
                        calls
                )
                localPortCallers
                ctx.callGraph
    in
    if localPortCallers == result then
        result

    else
        findIndirectLocalCallers ctx result


onlyExposedPortCalls : ModuleContext -> Dict String (Set ( ModuleName, String )) -> ModuleContext
onlyExposedPortCalls ({ projectContext } as ctx) portCallers =
    { ctx
        | projectContext =
            { projectContext
                | portCallers =
                    Dict.foldl
                        (\fnName calls acc ->
                            if isExposed ctx fnName then
                                Dict.insert ( ctx.moduleName, fnName ) calls acc

                            else
                                acc
                        )
                        projectContext.portCallers
                        portCallers
            }
    }


processTypeDefs : ModuleContext -> ModuleContext
processTypeDefs mod =
    { mod
        | typeDefs =
            mod.defsToProcess
                |> Dict.filter (\name _ -> isExposed mod name)
                |> TypeDef.resolveTypes
                    { lookup = mod.lookup
                    , defsInScope = mod.projectContext.typeDefs
                    , localDefs = mod.typeDefs
                    }
                |> Dict.union mod.typeDefs
    }


processPorts : ModuleContext -> ModuleContext
processPorts ({ projectContext } as mod) =
    let
        ( updatedPorts, updatedPortCallers ) =
            List.foldl
                (\( portName, portDirection, tyAnn ) ( ports, callers ) ->
                    ( Dict.insert ( mod.moduleName, portName )
                        { direction = portDirection
                        , type_ =
                            TypeDef.resolveTypeAnnotation
                                { lookup = mod.lookup
                                , defsInScope = projectContext.typeDefs
                                , localDefs = mod.typeDefs
                                , unresolved = mod.defsToProcess
                                }
                                tyAnn
                        }
                        ports
                    , Dict.insert ( mod.moduleName, portName )
                        (Set.singleton ( mod.moduleName, portName ))
                        callers
                    )
                )
                ( projectContext.ports, projectContext.portCallers )
                mod.portsToProcess
    in
    { mod
        | projectContext =
            { projectContext
                | ports = updatedPorts
                , portCallers = updatedPortCallers
            }
        , portsToProcess = []
    }


extractFlagsTypeAnnotation : ModuleContext -> Signature -> Maybe TypeDef.ConcreteTypeDef
extractFlagsTypeAnnotation mod sig =
    case
        TypeDef.resolveTypeAnnotation
            { lookup = mod.lookup
            , defsInScope = mod.projectContext.typeDefs
            , localDefs = mod.typeDefs
            , unresolved = mod.defsToProcess
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
    { withFlags = new.withFlags
    , entryPoints = Dict.union new.entryPoints previous.entryPoints
    , ports = Dict.union new.ports previous.ports
    , portCallers = Dict.union new.portCallers previous.portCallers
    , typeDefs = Dict.union new.typeDefs previous.typeDefs
    }


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        _ ->
            ( [], context )
