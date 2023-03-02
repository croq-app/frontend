module Croq.Data.Encode exposing (..)

import Croq.Data.Types exposing (..)
import Json.Encode exposing (Value, int, list, null, string)


type alias Encoder a =
    a -> Value



---
--- Atomic types
---


id : Encoder Id
id =
    string


name : Encoder Name
name =
    string


person : Encoder Person
person =
    string


text : Encoder Text
text =
    string


richText : Encoder RichText
richText { src } =
    string src


beta : Encoder Beta
beta =
    string


grade : Encoder Grade
grade =
    string


year : Encoder Year
year =
    int


meter : Encoder Meter
meter =
    int


video : Encoder Video
video =
    string


slug : Encoder Slug
slug =
    string


url : Encoder Url
url =
    string


country : Encoder Country
country =
    string


loadingerror : Encoder LoadingError
loadingerror =
    string


image : Encoder Image
image =
    string


refPath : Encoder RefPath
refPath =
    list string


nullable : Encoder a -> Encoder (Maybe a)
nullable enc x =
    case x of
        Just v ->
            enc v

        Nothing ->
            null



---
--- Utilities
---


{-| Read a nullable string and normalize empty strings to Nothing\`s
-}
nullableString : Encoder (Maybe String)
nullableString =
    Maybe.withDefault "" >> string
