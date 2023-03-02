module Croq.Data.RouteType exposing (RouteType(..), decoder, encoder, parse, toString)

import Croq.Data.Util exposing (parsingDecoder, tagParser, tagRender)
import Json.Decode as D
import Json.Encode as E
import Croq.Data.Util exposing (withParseError)


type RouteType
    = Sport
    | Traditional
    | Mixed
    | MultiPitch


tagMap : List ( String, RouteType )
tagMap =
    [ ( "sport", Sport ), ( "trad", Traditional ), ( "mixed", Mixed ), ( "multi", MultiPitch ) ]


parse : String -> Maybe RouteType
parse =
    tagParser tagMap


toString : RouteType -> String
toString =
    tagRender tagMap


encoder : RouteType -> E.Value
encoder =
    toString >> E.string


decoder : D.Decoder RouteType
decoder =
    parsingDecoder (withParseError "invalid route type" parse)
