module Croq.Data.Direction exposing (..)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra


type Direction
    = NORTH
    | SOUTH
    | EAST
    | WEST


encoder : Direction -> E.Value
encoder =
    toString >> E.string


decoder : D.Decoder Direction
decoder =
    D.string |> D.andThen (\s -> parse s |> Maybe.Extra.unwrap (D.fail "invalid direction") D.succeed)


toString : Direction -> String
toString dir =
    case dir of
        NORTH ->
            "north"

        SOUTH ->
            "south"

        EAST ->
            "east"

        WEST ->
            "west"


parse : String -> Maybe Direction
parse dir =
    case String.toLower dir of
        "north" ->
            Just NORTH

        "south" ->
            Just SOUTH

        "east" ->
            Just EAST

        "west" ->
            Just WEST

        _ ->
            Nothing
