module Croq.Data.BoulderProblem exposing (BoulderProblem, decode, encode, isGraded, showGrade, showWithGrade, tags)

import Croq.Data.Climbable as Climbable exposing (Climbable)
import Croq.Data.Id as Id
import Croq.Data.Types exposing (..)
import Croq.Util exposing (maybeShow)
import Grades.Bouldering as Bouldering
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Maybe.Extra as Maybe


{-| Information about a boulder problem
-}
type alias BoulderProblem =
    Climbable Id.ProblemId Bouldering.Grade ()


{-| True if a grade is assigned
-}
isGraded : BoulderProblem -> Bool
isGraded =
    .grade >> (/=) Nothing


{-| Show the possibly empty grade field
-}
showGrade : { m | grade : Maybe Bouldering.Grade } -> String
showGrade =
    .grade >> maybeShow Bouldering.show


showWithGrade : BoulderProblem -> String
showWithGrade problem =
    problem.name ++ " (" ++ showGrade problem ++ ")"


tags : BoulderProblem -> List String
tags p =
    let
        highball =
            Maybe.filter (\x -> x >= 5) p.height
                |> Maybe.unwrap [] (\_ -> [ "highball" ])
    in
    highball ++ Climbable.tags p


decode : D.Decoder BoulderProblem
decode =
    Climbable.decode Id.decodeProblemId decodeGrade
        |> D.optional "extra" (D.succeed ()) ()


encode : BoulderProblem -> E.Value
encode problem =
    Climbable.encode Id.encodeProblemId encodeGrade [] problem


decodeGrade : D.Decoder Bouldering.Grade
decodeGrade =
    D.string
        |> D.andThen
            (\s ->
                Bouldering.parse s
                    |> Maybe.unwrap (D.fail "invalid grade") D.succeed
            )


encodeGrade : Bouldering.Grade -> E.Value
encodeGrade grade =
    E.string (Bouldering.show grade)
