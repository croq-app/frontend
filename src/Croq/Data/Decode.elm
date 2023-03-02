module Croq.Data.Decode exposing (..)

import Croq.Data.Types exposing (..)
import Json.Decode exposing (Decoder, int, list, map, maybe, string)
import Json.Decode.Pipeline exposing (optional)



---
--- Atomic types
---


id : Decoder Id
id =
    string


name : Decoder Name
name =
    string


person : Decoder Person
person =
    string


text : Decoder Text
text =
    string


richText : Decoder RichText
richText =
    string |> map Croq.Data.Types.richText


beta : Decoder Beta
beta =
    string


grade : Decoder Grade
grade =
    string


year : Decoder Year
year =
    int


meter : Decoder Meter
meter =
    int


video : Decoder Video
video =
    string


slug : Decoder Slug
slug =
    string


url : Decoder Url
url =
    string


country : Decoder Country
country =
    string


loadingerror : Decoder LoadingError
loadingerror =
    string


image : Decoder Image
image =
    string


refPath : Decoder RefPath
refPath =
    list string



---
--- Utilities
---


{-| Read a nullable string and normalize empty strings to Nothing\`s
-}
nullableString : Decoder (Maybe String)
nullableString =
    maybe string
        |> map
            (\s ->
                if s == Just "" then
                    Nothing

                else
                    s
            )


{-| Read an optional field with a nullable string
-}
nullableStringField : String -> Decoder (Maybe String -> b) -> Decoder b
nullableStringField fname =
    optional fname nullableString Nothing


{-| Read an optional field with a nullable value.

Use nullableStringField to normalize empty strings to Nothing\`s

-}
nullableField : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
nullableField fname dec =
    optional fname (maybe dec) Nothing
