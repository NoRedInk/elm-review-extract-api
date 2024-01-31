module Extract.APITest exposing (all)

import Extract.API exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Extract.API"
        [ test "should extract the simplest thing possible" <|
            \() ->
                """module A exposing (..)

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """{
  "A": {
    "ports": {},
    "flags": "unit"
  }
}
"""
        , test "should extract a literal record flag type" <|
            \() ->
                """module A exposing (..)

main : Program { some : String, other : String } () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """{
  "A": {
    "ports": {},
    "flags": {
      "type": "record",
      "fields": {
        "some": "string",
        "other": "string"
      }
    }
  }
}
"""
        , test "local type alias in flags" <|
            \() ->
                """module A exposing (..)

type alias Flags =
    { some : String, other : String }

main : Program Flags () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """{
  "A": {
    "ports": {},
    "flags": {
      "type": "record",
      "fields": {
        "some": "string",
        "other": "string"
      }
    }
  }
}
"""
        , test "fancy local type alias in flags" <|
            \() ->
                """
module A exposing (..)

type alias WithSome a =
    { a | some : String }


type alias WithOther b a =
    { a | other : b }


type alias Flags =
    WithSome (WithOther String {})


main : Program Flags () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {},
    "flags": {
      "type": "record",
      "fields": {
        "some": "string",
        "other": "string"
      }
    }
  }
}
"""
        , test "let's have a basic port" <|
            \() ->
                """
port module A exposing (..)

type alias WithSome a =
    { a | some : String }


type alias WithOther b a =
    { a | other : b }


type alias Flags =
    WithSome (WithOther String {})

port myPort : String -> Cmd msg

main : Program Flags () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, myPort "test" )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {
      "myPort": {
        "direction": "cmd",
        "type": "string"
      }
    },
    "flags": {
      "type": "record",
      "fields": {
        "some": "string",
        "other": "string"
      }
    }
  }
}
"""
        , test "indirect port call" <|
            \() ->
                """
port module A exposing (..)

port myPort : String -> Cmd msg

callIt : String -> String -> Cmd msg
callIt a b =
    myPort (a ++ b)

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, callIt "hello" "world" )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {
      "myPort": {
        "direction": "cmd",
        "type": "string"
      }
    },
    "flags": "unit"
  }
}
"""
        , test "a subscription" <|
            \() ->
                """
port module A exposing (..)

port myPort : (() -> msg) -> Sub msg

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> myPort identity
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {
      "myPort": {
        "direction": "sub",
        "type": "unit"
      }
    },
    "flags": "unit"
  }
}
"""
        , test "a subscription and a command" <|
            \() ->
                """
port module A exposing (..)

port myPort : (() -> msg) -> Sub msg

port myCmd : String -> Cmd msg

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, myCmd "hi" )
        , subscriptions = \\_ -> myPort identity
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {
      "myCmd": {
        "direction": "cmd",
        "type": "string"
      },
      "myPort": {
        "direction": "sub",
        "type": "unit"
      }
    },
    "flags": "unit"
  }
}
"""
        , test "A port called in a different module" <|
            \() ->
                Review.Test.runOnModules rule
                    [ """
port module A exposing (..)

port myCmd : String -> Cmd msg
""", """
module B exposing (..)

import A as SomeModule

main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, SomeModule.myCmd "hi" )
        , subscriptions = always Sub.none
        }
""" ]
                    |> Review.Test.expectDataExtract """
{
  "B": {
    "ports": {
      "myCmd": {"direction": "cmd", "type": "string"}
    },
    "flags": "unit"
  }
}
"""
        , test "A Program alias" <|
            \() ->
                """
module A exposing (..)

type alias MyProgram flags = Program flags () ()

main : MyProgram {}
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {},
    "flags": {"type": "record", "fields": {}}
  }
}
"""
        , test "a Msg type appears" <|
            \() ->
                """
module A exposing (..)

type Msg = Foo | Bar

main : Program () () Msg
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {},
    "flags": "unit"
  }
}
"""
        , test "A silly but common setup" <|
            \() ->
                Review.Test.runOnModules rule
                    [ """
module Msg exposing (..)

type Msg
    = Foo
    | Bar
""", """
module Model exposing (..)

type alias Model =
    { some : String }

initModel : Model
initModel =
    { some = "foo" }
""", """
module Flags exposing (..)

type alias Flags =
    { field : () }
""", """
module Main exposing (..)

import Msg exposing (Msg)
import Model exposing (Model)
import Flags exposing (Flags)

main : Program Flags Model Msg
main =
    Platform.worker
        { init = \\_ -> ( initModel, Cmd.none )
        , update = \\_ m -> ( m, SomeModule.myCmd "hi" )
        , subscriptions = always Sub.none
        }
""" ]
                    |> Review.Test.expectDataExtract """
{
  "Main": {
    "ports": {},
    "flags": {
      "type": "record",
      "fields": {"field": "unit"}
    }
  }
}
"""
        , test "More indirection" <|
            \() ->
                Review.Test.runOnModules rule
                    [ """
module Msg exposing (..)

type Msg
    = Foo
    | Bar
""", """
module Model exposing (..)

type alias Model =
    { some : String }

initModel : Model
initModel =
    { some = "foo" }
""", """
module Flags exposing (..)

type alias Flags =
    { field : () }
""", """
module MyProgram exposing (..)

import Flags exposing (Flags)

type Model a b = Model (a -> b)
type Msg a b = Msg (a -> b)

type alias Program foo bar baz =
    Platform.Program Flags (Model foo bar) (Msg bar baz)
""", """
module Main exposing (..)

import Msg exposing (Msg)
import Model exposing (Model)
import MyProgram

main : MyProgram.Program () Model Msg
main =
    Platform.worker
        { init = \\_ -> ( initModel, Cmd.none )
        , update = \\_ m -> ( m, SomeModule.myCmd "hi" )
        , subscriptions = always Sub.none
        }
""" ]
                    |> Review.Test.expectDataExtract """
{
  "Main": {
    "ports": {},
    "flags": {
      "type": "record",
      "fields": {"field": "unit"}
    }
  }
}
"""
        , test "a unbound type variable appears" <|
            \() ->
                """
module A exposing (..)

type Model a b = Its a

main : Program () (Model String any) ()
main =
    Platform.worker
        { init = \\_ -> ( Its "a string", Cmd.none )
        , update = \\_ m -> ( m, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectDataExtract """
{
  "A": {
    "ports": {},
    "flags": "unit"
  }
}
"""
        ]
