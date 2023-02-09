module Croq.Data.Attraction exposing (..)

import Croq.Data.Id as Id
import Croq.Data.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)


type alias Attraction =
    { id : Id.SectorId
    , name : String
    , description : String
    , howToAccess : String
    , location : LatLng
    }


decoder : D.Decoder Attraction
decoder =
    D.succeed Attraction
        |> D.required "id" Id.decodeSectorId
        |> D.required "name" D.string
        |> D.optional "description" D.string ""
        |> D.optional "how_to_access" D.string ""
        |> D.required "location" LatLng.decoder


encoder : Attraction -> E.Value
encoder { id, name, description, howToAccess, location } =
    E.object
        [ ( "id", Id.encodeSectorId id )
        , ( "name", E.string name )
        , ( "description", E.string description )
        , ( "how_to_access", E.string howToAccess )
        , ( "location", LatLng.encoder location )
        ]
