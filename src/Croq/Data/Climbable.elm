module Croq.Data.Climbable exposing (Climbable, decode, encode, tags)

import Croq.Data.ClimbingTechnique as ClimbingTechnique exposing (ClimbingTechnique)
import Croq.Data.Direction as Direction exposing (Direction)
import Croq.Data.Rating as Rating exposing (Rating)
import Croq.Data.Types exposing (..)
import Croq.Data.Warning as Warning exposing (Warning)
import Croq.Util exposing (iff)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)
import Maybe.Extra as Maybe


type alias Climbable id grade extra =
    { id : id
    , name : Name 
    -- , shortName: String
    , description : Text
    , grade : Maybe grade
    , rating : Maybe Rating
    , location : Maybe LatLng
    , faceTo : Maybe Direction
    , height : Maybe Meter
    , year : Maybe Year
    , firstAscent : Maybe Person
    , establishedBy : List Person
    , techniques : List ClimbingTechnique
    , warnings : List Warning
    , videos : List Video
    -- , images: List Image
    , betas : List Beta
    , extra : extra
    }


tags : Climbable id grade extra -> List String
tags obj =
    [ obj.rating |> Maybe.map Rating.tags
    , obj.faceTo |> Maybe.map (Direction.toString >> List.singleton)
    , Just <| List.map ClimbingTechnique.toString obj.techniques
    , Just <| List.map Warning.toString obj.warnings
    , iff (List.isEmpty obj.betas) Nothing (Just [ "betas" ])
    ]
        |> Maybe.values
        |> List.concat


decode : D.Decoder id -> D.Decoder grade -> D.Decoder (a -> Climbable id grade a)
decode id grade =
    let
        nullable field dec =
            D.optional field (D.maybe dec) Nothing

        optList field dec =
            D.optional field (D.list dec) []
    in
    D.succeed Climbable
        |> D.required "id" id
        |> D.required "name" D.string
        |> D.optional "description" D.string ""
        |> nullable "grade" grade
        |> nullable "rating" Rating.decoder
        |> nullable "location" LatLng.decoder
        |> nullable "face_to" Direction.decoder
        |> nullable "height" D.int
        |> nullable "year" D.int
        |> nullable "first_ascent" D.string
        |> optList "established_by" D.string
        |> optList "techniques" ClimbingTechnique.decoder
        |> optList "warnings" Warning.decoder
        |> optList "videos" D.string
        |> optList "betas" D.string


encode : (id -> E.Value) -> (grade -> E.Value) -> List ( String, E.Value ) -> Climbable id grade extra -> E.Value
encode id grade extra obj =
    let
        nullable =
            Maybe.unwrap E.null
    in
    E.object
        ([ ( "id", id obj.id )
         , ( "name", E.string obj.name )
         , ( "description", E.string obj.description )
         , ( "grade", nullable grade obj.grade )
         , ( "rating", nullable Rating.encoder obj.rating )
         , ( "location", nullable LatLng.encoder obj.location )
         , ( "height", nullable E.int obj.height )
         , ( "year", nullable E.int obj.year )
         , ( "first_ascent", nullable E.string obj.firstAscent )
         , ( "established_by", E.list E.string obj.establishedBy )
         , ( "face_to", nullable Direction.encoder obj.faceTo )
         , ( "techniques", E.list ClimbingTechnique.encoder obj.techniques )
         , ( "warnings", E.list Warning.encoder obj.warnings )
         , ( "videos", E.list E.string obj.videos )
         , ( "betas", E.list E.string obj.betas )
         ]
            ++ extra
        )
