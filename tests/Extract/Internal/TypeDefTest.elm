module Extract.Internal.TypeDefTest exposing (all)

import Dict
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAnnotation as Tann
import Expect
import Extract.Internal.TypeDef as T
import Review.ModuleNameLookupTable as MNLT
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Extract.Internal.TypeDef"
        [ test "Should handle the simplest of things" <|
            \() ->
                -- type alias Foo = ()
                resolveTypesSimple
                    (Dict.singleton "Foo" ( [], Tann.Unit ))
                    |> Expect.equal (Dict.singleton "Foo" (T.Concrete T.TUnit))
        , test "What happens with a tyvar?" <|
            \() ->
                -- type alias Bar a = a
                -- type alias Foo = Bar ()
                resolveTypesSimple
                    (Dict.fromList
                        [ ( "Bar", ( [ "a" ], Tann.GenericType "a" ) )
                        , ( "Foo"
                          , ( []
                            , Tann.Typed (Node.empty ( [], "Bar" ))
                                [ Node.empty Tann.Unit ]
                            )
                          )
                        ]
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete T.TUnit))
        , test "Let's handle a maybe" <|
            \() ->
                -- type alias Foo = Maybe ()
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Typed
                            (Node.empty ( [ "Maybe" ], "Maybe" ))
                            [ Node.empty Tann.Unit ]
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete (T.TMaybe T.TUnit)))
        , test "We can nest maybes!" <|
            \() ->
                -- type alias Foo = Maybe (Maybe ())
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Typed
                            (Node.empty ( [ "Maybe" ], "Maybe" ))
                            [ Node.empty
                                (Tann.Typed
                                    (Node.empty ( [ "Maybe" ], "Maybe" ))
                                    [ Node.empty Tann.Unit ]
                                )
                            ]
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete (T.TMaybe (T.TMaybe T.TUnit))))
        , test "Let's handle a list" <|
            \() ->
                -- type alias Foo = List ()
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Typed
                            (Node.empty ( [ "List" ], "List" ))
                            [ Node.empty Tann.Unit ]
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete (T.TList T.TUnit)))
        , test "Let's add a twist" <|
            \() ->
                -- type alias Foo a = Maybe (List a)
                -- type alias Bar = Foo String
                -- --> Bar : TMaybe (TList Tstring)
                resolveTypesSimple
                    (Dict.fromList
                        [ ( "Foo"
                          , ( [ "a" ]
                            , Tann.Typed
                                (Node.empty ( [ "Maybe" ], "Maybe" ))
                                [ Node.empty
                                    (Tann.Typed
                                        (Node.empty ( [], "List" ))
                                        [ Node.empty (Tann.GenericType "a") ]
                                    )
                                ]
                            )
                          )
                        , ( "Bar"
                          , ( []
                            , Tann.Typed (Node.empty ( [], "Foo" ))
                                [ Node.empty
                                    (Tann.Typed (Node.empty ( [], "String" )) [])
                                ]
                            )
                          )
                        ]
                    )
                    |> Dict.get "Bar"
                    |> Expect.equal (Just (T.Concrete (T.TMaybe (T.TList T.TString))))
        , test "Our first record" <|
            \() ->
                -- type alias Foo = { foo : () }
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Record [ Node.empty ( Node.empty "foo", Node.empty Tann.Unit ) ]
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete (T.TRecord [ ( "foo", T.TUnit ) ])))
        , test "Tuples enter the game" <|
            \() ->
                -- type alias Foo = ((), ())
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Tupled [ Node.empty Tann.Unit, Node.empty Tann.Unit ]
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete (T.TTuple [ T.TUnit, T.TUnit ])))
        , test "Thruples with vars" <|
            \() ->
                -- type alias Foo a b c = ((a, b), (b, c), (c, a))
                -- type alias Bar = Foo String Int ()
                resolveTypesSimple
                    (Dict.fromList
                        [ ( "Foo"
                          , ( [ "a", "b", "c" ]
                            , Tann.Tupled
                                [ Node.empty
                                    (Tann.Tupled
                                        [ Node.empty (Tann.GenericType "a")
                                        , Node.empty (Tann.GenericType "b")
                                        ]
                                    )
                                , Node.empty
                                    (Tann.Tupled
                                        [ Node.empty (Tann.GenericType "b")
                                        , Node.empty (Tann.GenericType "c")
                                        ]
                                    )
                                , Node.empty
                                    (Tann.Tupled
                                        [ Node.empty (Tann.GenericType "c")
                                        , Node.empty (Tann.GenericType "a")
                                        ]
                                    )
                                ]
                            )
                          )
                        , ( "Bar"
                          , ( []
                            , Tann.Typed
                                (Node.empty ( [], "Foo" ))
                                [ Node.empty (Tann.Typed (Node.empty ( [], "String" )) [])
                                , Node.empty (Tann.Typed (Node.empty ( [], "Int" )) [])
                                , Node.empty Tann.Unit
                                ]
                            )
                          )
                        ]
                    )
                    |> Dict.get "Bar"
                    |> Expect.equal
                        (Just
                            (T.Concrete
                                (T.TTuple
                                    [ T.TTuple [ T.TString, T.TInt ]
                                    , T.TTuple [ T.TInt, T.TUnit ]
                                    , T.TTuple [ T.TUnit, T.TString ]
                                    ]
                                )
                            )
                        )
        , test "An extensible record!" <|
            \() ->
                -- type alias Foo a = { a | it : () }
                -- type alias Bar = Foo {}
                -- --> Bar : { it : () }
                resolveTypesSimple
                    (Dict.fromList
                        [ ( "Foo"
                          , ( [ "a" ]
                            , Tann.GenericRecord
                                (Node.empty "a")
                                (Node.empty
                                    [ Node.empty ( Node.empty "it", Node.empty Tann.Unit )
                                    ]
                                )
                            )
                          )
                        , ( "Bar"
                          , ( []
                            , Tann.Typed (Node.empty ( [], "Foo" ))
                                [ Node.empty (Tann.Record []) ]
                            )
                          )
                        ]
                    )
                    |> Dict.get "Bar"
                    |> Expect.equal (Just (T.Concrete (T.TRecord [ ( "it", T.TUnit ) ])))
        , test "layercake" <|
            \() ->
                -- type alias Foo a = { a | stuff : String }
                -- type alias Bar a b = (List a, Foo b)
                -- type alias Baz = Bar (Maybe Int) { otherStuff : () }
                -- --> Baz : ( List (Maybe Int), { otherStuff : (), stuff : String } )
                resolveTypesSimple
                    (Dict.fromList
                        [ ( "Foo"
                          , ( [ "a" ]
                            , Tann.GenericRecord
                                (Node.empty "a")
                                (Node.empty
                                    [ Node.empty
                                        ( Node.empty "stuff"
                                        , Node.empty (Tann.Typed (Node.empty ( [], "String" )) [])
                                        )
                                    ]
                                )
                            )
                          )
                        , ( "Bar"
                          , ( [ "a", "b" ]
                            , Tann.Tupled
                                [ Node.empty
                                    (Tann.Typed
                                        (Node.empty ( [], "List" ))
                                        [ Node.empty (Tann.GenericType "a") ]
                                    )
                                , Node.empty
                                    (Tann.Typed
                                        (Node.empty ( [], "Foo" ))
                                        [ Node.empty (Tann.GenericType "b") ]
                                    )
                                ]
                            )
                          )
                        , ( "Baz"
                          , ( []
                            , Tann.Typed
                                (Node.empty ( [], "Bar" ))
                                [ Node.empty
                                    (Tann.Typed
                                        (Node.empty ( [], "Maybe" ))
                                        [ Node.empty
                                            (Tann.Typed (Node.empty ( [], "Int" )) [])
                                        ]
                                    )
                                , Node.empty
                                    (Tann.Record
                                        [ Node.empty
                                            ( Node.empty "otherStuff"
                                            , Node.empty Tann.Unit
                                            )
                                        ]
                                    )
                                ]
                            )
                          )
                        ]
                    )
                    |> Dict.get "Baz"
                    |> Expect.equal
                        (Just
                            (T.Concrete
                                (T.TTuple
                                    [ T.TList (T.TMaybe T.TInt)
                                    , T.TRecord
                                        [ ( "stuff", T.TString )
                                        , ( "otherStuff", T.TUnit )
                                        ]
                                    ]
                                )
                            )
                        )
        , test "Json.Encode.Value does things" <|
            \_ ->
                -- type alias Foo = Json.Encode.Value
                resolveTypesSimple
                    (Dict.singleton "Foo"
                        ( []
                        , Tann.Typed
                            (Node.empty ( [ "Json", "Encode" ], "Value" ))
                            []
                        )
                    )
                    |> Dict.get "Foo"
                    |> Expect.equal (Just (T.Concrete T.TJsonValue))
        , test "Custom types are okay" <|
            \_ ->
                -- type Foo a b
                -- type alias Bar = Foo String Int
                T.resolveTypes
                    { lookup = MNLT.createForTests [] []
                    , defsInScope = Dict.empty
                    , localDefs =
                        Dict.singleton "Foo"
                            (T.createCtor ( [], "Foo" ))
                    }
                    (Dict.singleton "Bar"
                        ( []
                        , Tann.Typed
                            (Node.empty ( [], "Foo" ))
                            [ Node.empty (Tann.Typed (Node.empty ( [], "String" )) [])
                            , Node.empty (Tann.Typed (Node.empty ( [], "Int" )) [])
                            ]
                        )
                    )
                    |> Dict.get "Bar"
                    |> Expect.equal
                        (Just
                            (T.Concrete
                                (T.TCustom ( [], "Foo" ) [ T.TString, T.TInt ])
                            )
                        )
        ]


resolveTypesSimple =
    T.resolveTypes
        { lookup = MNLT.createForTests [] []
        , defsInScope = Dict.empty
        , localDefs = Dict.empty
        }
