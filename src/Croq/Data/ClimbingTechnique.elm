module Croq.Data.ClimbingTechnique exposing (ClimbingTechnique, decoder, encoder, parse, toString)

import Croq.Data.Util exposing (parsingDecoder, withParseError)
import Json.Decode as D
import Json.Encode as E


type ClimbingTechnique
    = Slab
    | Overhang
    | Crimp
    | Opposition
    | Aresta_


encoder : ClimbingTechnique -> E.Value
encoder =
    toString >> E.string


decoder : D.Decoder ClimbingTechnique
decoder =
    parsingDecoder (withParseError "invalid climbing style" parse)


toString : ClimbingTechnique -> String
toString type_ =
    case type_ of
        Slab ->
            "slab"

        Overhang ->
            "overhang"

        Crimp ->
            "crimp"

        Opposition ->
            "opposition"

        Aresta_ ->
            "aresta_"


parse : String -> Maybe ClimbingTechnique
parse st =
    case String.toLower st of
        "slab" ->
            Just Slab

        "overhang" ->
            Just Overhang

        "crimp" ->
            Just Crimp

        "opposition" ->
            Just Opposition

        "aresta_" ->
            Just Aresta_

        _ ->
            Nothing
