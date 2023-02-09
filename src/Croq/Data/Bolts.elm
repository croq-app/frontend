module Croq.Data.Bolts exposing (..)

import Croq.Data.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import String.Extra as String


type alias Bolts =
    { n : Int
    , nAnchor : Int
    , properties : List Property
    }


type Property
    = FastClip
    | Chain
    | PAnchor
    | PBolt
    | Parabolt
    | Spit
    | Summit Text
    | SpecialProtection Text


encoder : Bolts -> E.Value
encoder bolts =
    E.object
        [ ( "n", E.int bolts.n )
        , ( "n_anchor", E.int bolts.nAnchor )
        , ( "properties", E.list encodeProperty bolts.properties )
        ]


decoder : D.Decoder Bolts
decoder =
    D.succeed Bolts
        |> D.required "n" D.int
        |> D.required "n_anchor" D.int
        |> D.optional "flags" (D.list decodeProperty) []


decodeProperty : D.Decoder Property
decodeProperty =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "fastclip" ->
                        D.succeed FastClip

                    "chain" ->
                        D.succeed Chain

                    "p_anchor" ->
                        D.succeed PAnchor

                    "p_bolt" ->
                        D.succeed PBolt

                    "parabolt" ->
                        D.succeed Parabolt

                    "spit" ->
                        D.succeed Spit

                    _ ->
                        if String.startsWith "summit" s then
                            D.succeed (String.rightOf "summit" s |> String.trim |> Summit)

                        else if String.startsWith "special" s then
                            D.succeed (String.rightOf "special" s |> String.trim |> SpecialProtection)

                        else
                            D.fail ("invalid flag: " ++ s)
            )


encodeProperty : Property -> E.Value
encodeProperty =
    propertyToString >> E.string


propertyToString : Property -> String
propertyToString prop =
    case prop of
        FastClip ->
            "fastclip"

        Chain ->
            "chain"

        PAnchor ->
            "p_anchor"

        PBolt ->
            "p_bolt"

        Parabolt ->
            "parabolt"

        Spit ->
            "spit"

        Summit txt ->
            "summit " ++ txt

        SpecialProtection txt ->
            "special " ++ txt


renderNumBolts : Bolts -> String
renderNumBolts { n, nAnchor, properties } =
    let
        base =
            String.fromInt n

        anchor =
            String.fromInt nAnchor
                ++ selectElem PAnchor properties "P" ""

        suffix =
            sortProperties properties
                |> List.map propertyToString
                |> String.join ", "
    in
    base ++ "+" ++ anchor ++ suffix


sortProperties : List Property -> List Property
sortProperties lst =
    lst
