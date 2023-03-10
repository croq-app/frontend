module Croq.Data.Sector exposing (Kind(..), Sector, Tag(..), decoder, encoder, parseTag, renderTag)

import Croq.Data.BoulderFormation as BoulderFormation exposing (BoulderFormation)
import Croq.Data.Decode as D
import Croq.Data.Encode as E
import Croq.Data.Id as Id
import Croq.Data.Route as Route exposing (Route)
import Croq.Data.Types exposing (..)
import Croq.Data.Util exposing (normalizeShortName, parsingDecoder, renderingEncoder, tagParser, tagRender, withParseError)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import LatLng as LatLng exposing (LatLng)


type alias Sector =
    { id : Id.SectorId
    , name : Name
    , shortName : Name
    , kind : Kind
    , location : LatLng
    , description : Maybe RichText
    , howToAccess : Maybe RichText
    , routes : List Route
    , boulders : List BoulderFormation
    , tags : List Tag
    }


type Tag
    = EasyAccess
    | RescueWarning
    | Democratic


type Kind
    = Bouldering
    | Climbing
    | Mixed


tagMap : List ( String, Tag )
tagMap =
    [ ( "easy_access", EasyAccess )
    , ( "democratic", Democratic )
    , ( "bad_rescue", RescueWarning )
    ]


kindMap : List ( String, Kind )
kindMap =
    [ ( "route", Climbing ), ( "boulder", Bouldering ) ]


parseTag : String -> Maybe Tag
parseTag =
    String.toLower >> tagParser tagMap


renderTag : Tag -> String
renderTag =
    tagRender tagMap


parseKind : String -> Maybe Kind
parseKind =
    String.toLower >> tagParser kindMap


renderKind : Kind -> String
renderKind =
    tagRender kindMap


decoder : D.Decoder Sector
decoder =
    D.succeed Sector
        |> D.required "id" Id.decodeSectorId
        |> D.required "name" D.string
        |> D.optional "shortName" D.string ""
        |> D.required "kind" kindDecoder
        |> D.required "location" LatLng.decoder
        |> D.nullableField "description" D.richText
        |> D.nullableField "how_to_access" D.richText
        |> D.optional "routes" (D.list Route.decoder) []
        |> D.optional "boulders" (D.list BoulderFormation.decoder) []
        |> D.optional "tags" (D.list tagDecoder) []
        |> D.map normalizeShortName


encoder : Sector -> E.Value
encoder sector =
    E.object
        [ ( "id", Id.encodeSectorId sector.id )
        , ( "name", E.string sector.name )
        , ( "short_name", E.string sector.shortName )
        , ( "kind", kindEncoder sector.kind )
        , ( "location", LatLng.encoder sector.location )
        , ( "description", E.nullable E.richText sector.description )
        , ( "how_to_access", E.nullable E.richText sector.howToAccess )
        , ( "routes", E.list Route.encoder sector.routes )
        , ( "boulders", E.list BoulderFormation.encoder sector.boulders )
        , ( "tags", E.list tagEncoder sector.tags )
        ]


tagDecoder : D.Decoder Tag
tagDecoder =
    parsingDecoder (withParseError "invalid tag" parseTag)


tagEncoder : Tag -> E.Value
tagEncoder =
    renderingEncoder renderTag


kindDecoder : D.Decoder Kind
kindDecoder =
    parsingDecoder (withParseError "invalid kind" parseKind)


kindEncoder : Kind -> E.Value
kindEncoder =
    renderingEncoder renderKind
