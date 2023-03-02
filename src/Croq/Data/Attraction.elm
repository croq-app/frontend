module Croq.Data.Attraction exposing (..)

import Croq.Data.Decode as D
import Croq.Data.Encode as E
import Croq.Data.Id as Id
import Croq.Data.Types exposing (..)
import Croq.Data.Util exposing (normalizeShortName)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)


type alias Attraction =
    { id : Id.SectorId
    , name : Name
    , shortName : Name
    , location : LatLng
    , description : RichText
    , howToAccess : Maybe RichText
    }


decoder : D.Decoder Attraction
decoder =
    D.succeed Attraction
        |> D.required "id" Id.decodeSectorId
        |> D.required "name" D.name
        |> D.optional "short_name" D.name ""
        |> D.required "location" LatLng.decoder
        |> D.required "description" D.richText
        |> D.nullableField "how_to_access" D.richText
        |> D.map normalizeShortName


encoder : Attraction -> E.Value
encoder { id, name, description, howToAccess, location } =
    E.object
        [ ( "id", Id.encodeSectorId id )
        , ( "name", E.string name )
        , ( "short_name", E.string name )
        , ( "location", LatLng.encoder location )
        , ( "description", E.richText description )
        , ( "how_to_access", E.nullable E.richText howToAccess )
        ]
