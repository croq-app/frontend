module Croq.Data.Warning exposing (..)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra


type Warning
    = Bees
    | LooseRocks


encoder : Warning -> E.Value
encoder =
    toString >> E.string


decoder : D.Decoder Warning
decoder =
    D.string
        |> D.andThen
            (\s ->
                parse s
                    |> Maybe.Extra.unwrap (D.fail "invalid state") D.succeed
            )


toString : Warning -> String
toString type_ =
    case type_ of
        Bees ->
            "bees"

        LooseRocks ->
            "loose rocks"


parse : String -> Maybe Warning
parse st =
    case String.toLower st of
        "bees" ->
            Just Bees

        "loose rocks" ->
            Just LooseRocks

        _ ->
            Nothing
