module Croq.Data.Id exposing (..)

import Croq.Data.Types exposing (..)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Id a =
    { parent : a, slug : Slug }


type alias CountryId =
    { parent : (), slug : Slug }


type alias RegionId =
    { parent : CountryId, slug : Slug }


type alias SectorId =
    { parent : RegionId, slug : Slug }


type alias ElemId =
    { parent : SectorId, slug : Slug }


type alias ProblemId =
    { parent : ElemId, slug : Slug }


countryId : Slug -> Id ()
countryId =
    Id ()


regionId : Slug -> Slug -> RegionId
regionId =
    countryId >> RegionId


sectorId : Slug -> Slug -> Slug -> SectorId
sectorId a b c =
    SectorId (regionId a b) c


elemId : Slug -> Slug -> Slug -> Slug -> ElemId
elemId a b c d =
    ElemId (sectorId a b c) d


problemId : Slug -> Slug -> Slug -> Slug -> Slug -> ProblemId
problemId a b c d e =
    ProblemId (elemId a b c d) e


{-| Get element with the given id from list
-}
elemFromId : id -> List { m | id : id } -> Maybe { m | id : id }
elemFromId id elems =
    case elems of
        x :: xs ->
            if x.id == id then
                Just x

            else
                elemFromId id xs

        [] ->
            Nothing



--- DECODERS ------------------------------------------------------------------


decodeRegionId : D.Decoder RegionId
decodeRegionId =
    D.string
        |> D.andThen
            (\st ->
                case String.split "/" st of
                    [ a, b ] ->
                        D.succeed (RegionId (countryId a) b)

                    _ ->
                        D.fail <| "required id in the format <country>/<slug>, got " ++ st
            )


decodeSectorId : D.Decoder SectorId
decodeSectorId =
    D.string
        |> D.andThen
            (\st ->
                case String.split "/" st of
                    [ a, b, c ] ->
                        D.succeed (SectorId (RegionId (countryId a) b) c)

                    _ ->
                        D.fail <| "required id in the format <country>/<region>/<slug>, got " ++ st
            )


decodeElemId : D.Decoder ElemId
decodeElemId =
    D.string
        |> D.andThen
            (\st ->
                case String.split "/" st of
                    [ a, b, c, d ] ->
                        D.succeed (ElemId (SectorId (RegionId (countryId a) b) c) d)

                    _ ->
                        D.fail <| "required id in the format <country>/<region>/<sector>/<slug>, got " ++ st
            )


decodeProblemId : D.Decoder ProblemId
decodeProblemId =
    D.string
        |> D.andThen
            (\st ->
                case String.split "/" st of
                    [ a, b, c, d, e ] ->
                        D.succeed (ProblemId (ElemId (SectorId (RegionId (countryId a) b) c) d) e)

                    _ ->
                        D.fail <| "required id in the format <country>/<region>/<sector>/<boulder>/<slug>, got " ++ st
            )



--- ENCODERS ------------------------------------------------------------------


showRegionId : RegionId -> String
showRegionId id =
    id.parent.slug ++ "/" ++ id.slug


encodeRegionId : RegionId -> E.Value
encodeRegionId =
    showRegionId >> E.string


showSectorId : SectorId -> String
showSectorId id =
    String.join "/" [ id.parent.parent.slug, id.parent.slug, id.slug ]


encodeSectorId : SectorId -> E.Value
encodeSectorId =
    showSectorId >> E.string


showElemId : ElemId -> String
showElemId id =
    String.join "/" [ id.parent.parent.parent.slug, id.parent.parent.slug, id.parent.slug, id.slug ]


encodeElemId : ElemId -> E.Value
encodeElemId =
    showElemId >> E.string


showProblemId : ProblemId -> String
showProblemId id =
    String.join "/" [ id.parent.parent.parent.parent.slug, id.parent.parent.parent.slug, id.parent.parent.slug, id.parent.slug, id.slug ]


encodeProblemId : ProblemId -> E.Value
encodeProblemId =
    showProblemId >> E.string
