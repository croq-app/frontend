module Croq.Data.CompletionState exposing (CompletionState(..), decoder, encoder, parse, toString)

import Croq.Data.Util exposing (parsingDecoder, tagParser, tagRender)
import Json.Decode as D
import Json.Encode as E
import Croq.Data.Util exposing (withParseError)


type CompletionState
    = IsFinished
    | IsProject
    | IsUnfinished


tagMap : List ( String, CompletionState )
tagMap =
    [ ( "finished", IsFinished ), ( "project", IsProject ), ( "unfinished", IsUnfinished ) ]


parse : String -> Maybe CompletionState
parse =
    tagParser tagMap


toString : CompletionState -> String
toString =
    tagRender tagMap


encoder : CompletionState -> E.Value
encoder =
    toString >> E.string


decoder : D.Decoder CompletionState
decoder =
    parsingDecoder (withParseError "invalid route type" parse)
