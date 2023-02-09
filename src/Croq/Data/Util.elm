module Croq.Data.Util exposing (..)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra


parsingDecoder : String -> (String -> Maybe a) -> D.Decoder a
parsingDecoder error parse =
    D.string
        |> D.andThen
            (\s ->
                parse s
                    |> Maybe.Extra.unwrap (D.fail (error ++ ": \"" ++ s ++ "\"")) D.succeed
            )


renderingEncoder : (a -> String) -> a -> E.Value
renderingEncoder show value =
    E.string (show value)


tagParser : List ( a, b ) -> a -> Maybe b
tagParser tags src =
    case tags of
        ( x, out ) :: xs ->
            if x == src then
                Just out

            else
                tagParser xs src

        _ ->
            Nothing


tagRender : List ( String, b ) -> b -> String
tagRender tags obj =
    case tags of
        ( st, out ) :: xs ->
            if out == obj then
                st

            else
                tagRender xs obj

        _ ->
            "invalid"


tagRenderOr : List ( String, b ) -> (b -> String) -> b -> String
tagRenderOr tags fallback obj =
    case tags of
        ( st, out ) :: xs ->
            if out == obj then
                st

            else
                tagRenderOr xs fallback obj

        _ ->
            fallback obj


normalizeShortName : { a | name : String, shortName : String } -> { a | name : String, shortName : String }
normalizeShortName m =
    case ( m.name, m.shortName ) of
        ( "", _ ) ->
            { m | name = m.shortName }

        ( _, "" ) ->
            { m | shortName = m.name }

        _ ->
            m
