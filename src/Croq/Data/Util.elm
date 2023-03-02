module Croq.Data.Util exposing (..)

import Json.Decode as D
import Json.Encode as E


parsingDecoder : (String -> Result String a) -> D.Decoder a
parsingDecoder parse =
    D.string
        |> D.andThen
            (\s ->
                case parse s of
                    Ok x ->
                        D.succeed x

                    Err e ->
                        D.fail e
            )


withParseError : String -> (a -> Maybe b) -> (a -> Result String b)
withParseError err f x =
    Result.fromMaybe err (f x)


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


{-| Normalize records with both a name and shortName by setting any empty value from the other field.
-}
normalizeShortName : { a | name : String, shortName : String } -> { a | name : String, shortName : String }
normalizeShortName m =
    parseShortName m |> Result.withDefault m


{-| Similar to normalizeShortName, but return an error if both elements are empty
-}
parseShortName : { a | name : String, shortName : String } -> Result String { a | name : String, shortName : String }
parseShortName m =
    case ( m.name, m.shortName ) of
        ( "", "" ) ->
            Err "both name and shortName are empty"

        ( "", _ ) ->
            Ok { m | name = m.shortName }

        ( _, "" ) ->
            Ok { m | shortName = m.name }

        _ ->
            Ok m
