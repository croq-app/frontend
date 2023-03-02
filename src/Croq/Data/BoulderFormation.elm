module Croq.Data.BoulderFormation exposing
    ( BoulderFormation
    , BoulderInfo
    , decoder
    , encoder
    , infos
    , isGraded
    )

import Croq.Data.BoulderProblem as BoulderProblem exposing (BoulderProblem)
import Croq.Data.Decode as D
import Croq.Data.Encode as E
import Croq.Data.Id as Id
import Croq.Data.Types exposing (..)
import Croq.Data.Util exposing (normalizeShortName)
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E


{-| Formation is a large rock/boulder with some boulder problems
-}
type alias BoulderFormation =
    { id : Id.ElemId
    , name : Name
    , shortName : Name
    , problems : List BoulderProblem
    }


{-| Relate a boulder problem with its corresponding formation
-}
type alias BoulderInfo =
    { problem : BoulderProblem
    , formation : BoulderFormation
    }


infos : BoulderFormation -> List BoulderInfo
infos formation =
    List.map (\p -> BoulderInfo p formation) formation.problems


{-| True if a grade is assigned
-}
isGraded : BoulderInfo -> Bool
isGraded =
    .problem >> BoulderProblem.isGraded


decoder : D.Decoder BoulderFormation
decoder =
    D.succeed BoulderFormation
        |> D.required "id" Id.decodeElemId
        |> D.required "name" D.name
        |> D.optional "short_name" D.name ""
        |> D.optional "problems" (D.list BoulderProblem.decode) []
        |> D.map normalizeShortName


encoder : BoulderFormation -> E.Value
encoder formation =
    E.object
        [ ( "id", Id.encodeElemId formation.id )
        , ( "name", E.name formation.name )
        , ( "short_name", E.name formation.shortName )
        , ( "problems", E.list BoulderProblem.encode formation.problems )
        ]
