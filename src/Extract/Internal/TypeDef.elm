module Extract.Internal.TypeDef exposing
    ( ConcreteTypeDef(..)
    , TypeDef(..)
    , createCtor
    , encode
    , materialize
    , resolveTypeAnnotation
    , resolveTypes
    , toCodable
    )

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TAnn exposing (TypeAnnotation)
import Extract.Internal.Util exposing (..)
import Json.Encode as Encode
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)



-- According to the docs, ports and flags only support the following types:
--   Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, tuples, records, and
--   JSON values.
--
-- This is true, but... You can still do something like this:
--
--    type alias Abstract1 a b = (a, b)
--    type alias Abstract2 x y = { foo : x, b : Abstract1 String y }
--    port foo : Abstract2 (List Int) Float -> Cmd msg
--
-- Since the type in the definition of `foo` is concrete, we're all good!
--
-- On top of that, we may also be dealing with types imported from elsewhere and
-- composed in different places. So, we need some abstraction to pass these
-- things around!
--
-- To make matters ever so slightly more confusing, extensible records cannot
-- appear in the types passed to ports, at all... But they _can_ appear in
-- flags! Why? No one knows.
--
-- When dealing with flags, it's not enough to "extract" from `main : Program
-- flags model msg`; because there might be aliases for `Program`. So, we need
-- to resolve the entire type signature for `main`, and then check whether that
-- gives us a `Program`. This also means we can't ignore things that don't fit
-- neatly into "only concrete types" because now we are also dealing with the
-- annoying reality of unbound type variables and function type signatures. A
-- signature like the below is perfectly valid in Elm:
--
--     main : Program Flags (a -> b) (a -> b)
--
-- That said, the flags do need to have a nice, concrete, "simple" type.
--
-- So, we end up working with three abstractions:
-- - `CodableTypeDef`, which are those we expect for flags and ports, and are
--   encodable to json
-- - `ConcreteTypeDef` are types that appear in type signatures for functions,
--   like `main`. They are "concrete", but could still have unbound variables.
-- - `TypeDef` represents either a `ConcreteTypeDef`, or a type that requires
--   more parameters before maybe turning into a concrete typedef.
--
-- I suspect we may be able to simplify some of this, but that's what we're
-- working with for the time being.


type TypeDef
    = Concrete ConcreteTypeDef
    | WithArgs (List ConcreteTypeDef -> Maybe TypeDef)


type ConcreteTypeDef
    = TUnit
    | TInt
    | TFloat
    | TBool
    | TString
    | TMaybe ConcreteTypeDef
    | TList ConcreteTypeDef
    | TArray ConcreteTypeDef
    | TTuple (List ConcreteTypeDef)
    | TRecord (List ( String, ConcreteTypeDef ))
    | TJsonValue
    | TCustom ( ModuleName, String ) (List ConcreteTypeDef)
    | TFuncApp ConcreteTypeDef ConcreteTypeDef
    | TUnbound String


type CodableTypeDef
    = CUnit
    | CInt
    | CFloat
    | CBool
    | CString
    | CMaybe CodableTypeDef
    | CList CodableTypeDef
    | CArray CodableTypeDef
    | CTuple (List CodableTypeDef)
    | CRecord (List ( String, CodableTypeDef ))
    | CJsonValue


toConcrete : TypeDef -> Maybe ConcreteTypeDef
toConcrete typeDef =
    case typeDef of
        Concrete v ->
            Just v

        _ ->
            Nothing


toCodable : ConcreteTypeDef -> Maybe CodableTypeDef
toCodable typeDef =
    case typeDef of
        TUnit ->
            Just CUnit

        TInt ->
            Just CInt

        TFloat ->
            Just CFloat

        TBool ->
            Just CBool

        TString ->
            Just CString

        TMaybe inner ->
            Maybe.map CMaybe (toCodable inner)

        TList inner ->
            Maybe.map CList (toCodable inner)

        TArray inner ->
            Maybe.map CArray (toCodable inner)

        TTuple inners ->
            Maybe.map CTuple (mtraverse toCodable inners)

        TRecord fields ->
            fields
                |> List.map
                    (\( field, inner ) ->
                        Maybe.map (Tuple.pair field) (toCodable inner)
                    )
                |> msequence
                |> Maybe.map CRecord

        TJsonValue ->
            Just CJsonValue

        TCustom _ _ ->
            Nothing

        TFuncApp _ _ ->
            Nothing

        TUnbound _ ->
            Nothing


encode : CodableTypeDef -> Encode.Value
encode typeDef =
    case typeDef of
        CUnit ->
            Encode.string "unit"

        CInt ->
            Encode.string "int"

        CFloat ->
            Encode.string "float"

        CBool ->
            Encode.string "bool"

        CString ->
            Encode.string "string"

        CMaybe content ->
            Encode.object
                [ ( "type", Encode.string "maybe" )
                , ( "content", encode content )
                ]

        CList content ->
            Encode.object
                [ ( "type", Encode.string "list" )
                , ( "content", encode content )
                ]

        CArray content ->
            Encode.object
                [ ( "type", Encode.string "array" )
                , ( "content", encode content )
                ]

        CTuple items ->
            Encode.object
                [ ( "type", Encode.string "tuple" )
                , ( "items", Encode.list encode items )
                ]

        CRecord items ->
            Encode.object
                [ ( "type", Encode.string "record" )
                , ( "fields", Encode.object (List.map (Tuple.mapSecond encode) items) )
                ]

        CJsonValue ->
            Encode.string "json"


type alias Context =
    { lookup : ModuleNameLookupTable
    , defsInScope : Dict ( ModuleName, String ) TypeDef
    , localDefs : Dict String TypeDef
    }


type alias Acc =
    { results : Dict String TypeDef
    , defs : Dict String ( List String, TypeAnnotation )
    }


resolveTypeAnnotation :
    { lookup : ModuleNameLookupTable
    , defsInScope : Dict ( ModuleName, String ) TypeDef
    , localDefs : Dict String TypeDef
    , unresolved : Dict String ( List String, TypeAnnotation )
    }
    -> TypeAnnotation
    -> Maybe ConcreteTypeDef
resolveTypeAnnotation { lookup, defsInScope, localDefs, unresolved } tyAnn =
    resolveTyAnn
        { lookup = lookup, defsInScope = defsInScope, localDefs = localDefs }
        tyAnn
        { results = localDefs, defs = unresolved }
        |> Tuple.first
        |> materialize


resolveTypes : Context -> Dict String ( List String, TypeAnnotation ) -> Dict String TypeDef
resolveTypes context defs =
    resolveTypesHelper context { results = Dict.empty, defs = defs }


resolveTypesHelper : Context -> Acc -> Dict String TypeDef
resolveTypesHelper context acc =
    case Dict.toList acc.defs of
        [] ->
            acc.results

        ( tyName, ( tyVars, tyAnn ) ) :: _ ->
            resolveType context tyName tyVars tyAnn { acc | defs = Dict.remove tyName acc.defs }
                |> resolveTypesHelper context


resolveType : Context -> String -> List String -> TypeAnnotation -> Acc -> Acc
resolveType context tyName tyVars tyAnn acc =
    let
        ( resultingType, newAcc ) =
            resolveTyAnn context tyAnn acc
    in
    case withTyVars tyVars resultingType of
        Nothing ->
            newAcc

        Just typeDef ->
            { newAcc | results = Dict.insert tyName typeDef newAcc.results }


type Bound a
    = Bound (Dict String ConcreteTypeDef -> Maybe a)


pure : a -> Bound a
pure a =
    Bound (\_ -> Just a)


map : (a -> b) -> Bound a -> Bound b
map f (Bound a) =
    Bound (a >> Maybe.map f)


map2 : (a -> b -> c) -> Bound a -> Bound b -> Bound c
map2 f (Bound a) (Bound b) =
    Bound
        (\bindings ->
            Maybe.map2 f (a bindings) (b bindings)
        )


try2 : (a -> b -> Maybe c) -> Bound a -> Bound b -> Bound c
try2 f (Bound a) (Bound b) =
    Bound
        (\bindings ->
            Maybe.map2 f (a bindings) (b bindings)
                |> Maybe.andThen identity
        )


invalid : Bound a
invalid =
    Bound (\_ -> Nothing)


fromBinding : String -> Bound ConcreteTypeDef
fromBinding binding =
    Bound (Dict.get binding >> Maybe.withDefault (TUnbound binding) >> Just)


materialize : Bound ConcreteTypeDef -> Maybe ConcreteTypeDef
materialize =
    withTyVars [] >> Maybe.andThen toConcrete


withTyVars : List String -> Bound ConcreteTypeDef -> Maybe TypeDef
withTyVars tyVars (Bound fn) =
    case tyVars of
        [] ->
            Maybe.map Concrete (fn Dict.empty)

        _ ->
            Just <|
                WithArgs <|
                    \args ->
                        List.map2 Tuple.pair tyVars args
                            |> Dict.fromList
                            |> fn
                            |> Maybe.map Concrete


tryApply : TypeDef -> List ConcreteTypeDef -> Maybe TypeDef
tryApply td ctds =
    case ( td, ctds ) of
        ( Concrete _, [] ) ->
            Just td

        ( Concrete _, _ ) ->
            Nothing

        ( WithArgs f, _ ) ->
            f ctds


try : (a -> Maybe b) -> Bound a -> Bound b
try f (Bound a) =
    Bound (a >> Maybe.andThen f)


andThen : (a -> Bound b) -> Bound a -> Bound b
andThen next (Bound a) =
    Bound
        (\bindings ->
            case a bindings of
                Just concrete ->
                    let
                        (Bound b) =
                            next concrete
                    in
                    b bindings

                Nothing ->
                    Nothing
        )


resolveTyAnn : Context -> TypeAnnotation -> Acc -> ( Bound ConcreteTypeDef, Acc )
resolveTyAnn context tyAnn acc =
    case tyAnn of
        TAnn.GenericType name ->
            ( fromBinding name, acc )

        TAnn.Unit ->
            ( pure TUnit, acc )

        TAnn.Tupled inners ->
            List.foldr
                (\inner ( resolved, acc_ ) ->
                    resolveTyAnn context (Node.value inner) acc_
                        |> Tuple.mapFirst (map2 (\xs a -> a :: xs) resolved)
                )
                ( pure [], acc )
                inners
                |> Tuple.mapFirst (map TTuple)

        TAnn.Typed typName args ->
            let
                ( argTypes, acc2 ) =
                    List.foldr
                        (\inner ( resolved, acc_ ) ->
                            resolveTyAnn context (Node.value inner) acc_
                                |> Tuple.mapFirst (map2 (\xs a -> a :: xs) resolved)
                        )
                        ( pure [], acc )
                        args
            in
            getTypeCtor context typName acc2
                |> Tuple.mapFirst (flip (try2 tryApply) argTypes)
                |> Tuple.mapFirst (try toConcrete)

        TAnn.Record recordDef ->
            resolveRecordDef context recordDef ( pure [], acc )
                |> Tuple.mapFirst (map TRecord)

        TAnn.GenericRecord genericPart recordDef ->
            fromBinding (Node.value genericPart)
                |> andThen
                    (\concrete ->
                        case concrete of
                            TRecord fields ->
                                pure fields

                            _ ->
                                invalid
                    )
                |> flip Tuple.pair acc
                |> resolveRecordDef context (Node.value recordDef)
                |> Tuple.mapFirst (map TRecord)

        TAnn.FunctionTypeAnnotation l r ->
            let
                ( bl, acc1 ) =
                    resolveTyAnn context (Node.value l) acc

                ( br, acc2 ) =
                    resolveTyAnn context (Node.value r) acc1
            in
            ( map2 TFuncApp bl br, acc2 )


createCtor : ( ModuleName, String ) -> TypeDef
createCtor mName =
    WithArgs <| \args -> Just (Concrete (TCustom mName args))


withSingle : (ConcreteTypeDef -> ConcreteTypeDef) -> TypeDef
withSingle toType =
    WithArgs <|
        \args ->
            case args of
                [ arg ] ->
                    Just (Concrete (toType arg))

                _ ->
                    Nothing


getTypeCtor : Context -> Node ( ModuleName, String ) -> Acc -> ( Bound TypeDef, Acc )
getTypeCtor context ((Node _ ( moduleName, typName )) as typNode) acc =
    case ( ModuleNameLookupTable.moduleNameFor context.lookup typNode, typName ) of
        ( Just [ "Maybe" ], "Maybe" ) ->
            ( pure (withSingle TMaybe), acc )

        ( Just [ "List" ], "List" ) ->
            ( pure (withSingle TList), acc )

        ( Just [ "Array" ], "Array" ) ->
            ( pure (withSingle TArray), acc )

        ( Just [ "String" ], "String" ) ->
            ( pure (Concrete TString), acc )

        ( Just [ "Basics" ], "Int" ) ->
            ( pure (Concrete TInt), acc )

        ( Just [ "Basics" ], "Float" ) ->
            ( pure (Concrete TFloat), acc )

        ( Just [ "Basics" ], "Bool" ) ->
            ( pure (Concrete TBool), acc )

        ( Just [ "Json", "Encode" ], "Value" ) ->
            ( pure (Concrete TJsonValue), acc )

        ( Just [ "Json", "Decode" ], "Value" ) ->
            -- technically a type alias for Json.Encode.Value
            -- but we're not parsing dependencies (yet) so need
            -- to hardcode it!
            ( pure (Concrete TJsonValue), acc )

        -- For tests, where we pass a rather useless ModuleName lookup table
        ( Nothing, "Maybe" ) ->
            ( pure (withSingle TMaybe), acc )

        ( Nothing, "List" ) ->
            ( pure (withSingle TList), acc )

        ( Nothing, "Array" ) ->
            ( pure (withSingle TArray), acc )

        ( Nothing, "String" ) ->
            ( pure (Concrete TString), acc )

        ( Nothing, "Int" ) ->
            ( pure (Concrete TInt), acc )

        ( Nothing, "Float" ) ->
            ( pure (Concrete TFloat), acc )

        ( Nothing, "Bool" ) ->
            ( pure (Concrete TBool), acc )

        ( Nothing, "Value" ) ->
            ( pure (Concrete TJsonValue), acc )

        ( Nothing, "Program" ) ->
            ( pure (createCtor ( [ "Platform" ], "Program" )), acc )

        ( Just ((_ :: _) as someModule), _ ) ->
            case Dict.get ( someModule, typName ) context.defsInScope of
                Just typeDef ->
                    ( pure typeDef, acc )

                Nothing ->
                    -- If we encounter something we don't know, consider
                    -- it an opaque custom type.
                    ( pure (createCtor ( someModule, typName )), acc )

        ( _, t ) ->
            case
                Dict.get typName context.localDefs
                    |> orElse (Dict.get ( [], typName ) context.defsInScope)
                    |> orElse (Dict.get typName acc.results)
            of
                Just typeDef ->
                    ( pure typeDef, acc )

                Nothing ->
                    case Dict.get typName acc.defs of
                        Just ( tyVars, tyAnn ) ->
                            resolveType context typName tyVars tyAnn { acc | defs = Dict.remove typName acc.defs }
                                |> getTypeCtor context typNode

                        Nothing ->
                            -- If all else fails, give up
                            ( invalid, acc )


orElse : Maybe a -> Maybe a -> Maybe a
orElse snd fst =
    if fst == Nothing then
        snd

    else
        fst


resolveRecordDef :
    Context
    -> List (Node TAnn.RecordField)
    -> ( Bound (List ( String, ConcreteTypeDef )), Acc )
    -> ( Bound (List ( String, ConcreteTypeDef )), Acc )
resolveRecordDef context fields acc =
    List.foldr
        (\field ( resolved, acc_ ) ->
            let
                ( fieldName, fieldAnn ) =
                    Node.value field
            in
            resolveTyAnn context (Node.value fieldAnn) acc_
                |> Tuple.mapFirst (map2 (\xs a -> ( Node.value fieldName, a ) :: xs) resolved)
        )
        acc
        fields
