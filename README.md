# `elm-review-extract-api`

An elm-review rule with extractor to extract the public interface (that is ports, their types, and flags) of all entrypoints in an Elm application.

## Say what now?

Imagine you have this beautiful application:

``` elm
port module Main exposing (main)


port send : { data : String, finished : Bool } -> Cmd msg


port recv : (List String -> msg) -> Sub msg


type alias Flags =
    { more : String
    , stuff : Int
    }


main : Program Flags () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), send { data = "hi", finished = False } )
        , subscriptions = \_ -> recv (\_ -> ())
        }
```

Now, with this rule configured, running `elm-review --report=json --extract` can yield us this extract:

``` json
{
    "Main": {
        "cmds": {
            "send": {
                "type": "record",
                "fields": {
                    "data": "string",
                    "finished": "bool"
                }
            }
        },
        "subs": {
            "recv": {
                "type": "list",
                "content": "string"
            }
        },
        "flags": {
            "type": "record",
            "fields": {
                "more": "string",
                "stuff": "int"
            }
        }
    }
}
```

Having this information isn't particularily valuable on its own, but it can be used for further processing, such as generating typescript definition files to ensure you use your app correctly!
