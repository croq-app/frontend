module Croq.Data.BoulderFormation exposing (BoulderFormation, BoulderInfo, decoder, encoder, isGraded, infos)

import Croq.Data.BoulderProblem as BoulderProblem exposing (BoulderProblem)
import Croq.Data.Id as Id
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E


{-| Formation is a large rock/boulder with some boulder problems
-}
type alias BoulderFormation =
    { id : Id.ElemId
    , name : String
    , shortName : String
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
        |> D.required "name" D.string
        |> D.required "short_name" D.string
        |> D.optional "problems" (D.list BoulderProblem.decode) []


encoder : BoulderFormation -> E.Value
encoder formation =
    E.object
        [ ( "id", Id.encodeElemId formation.id )
        , ( "name", E.string formation.name )
        , ( "short_name", E.string formation.shortName )
        , ( "problems", E.list BoulderProblem.encode formation.problems )
        ]
