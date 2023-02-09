module Croq.Data.Route exposing (Route, decoder, encoder, tags)

import Croq.Data.Bolts as NumBolts exposing (Bolts)
import Croq.Data.Climbable as Climbable exposing (Climbable)
import Croq.Data.CompletionState as CompletionState exposing (CompletionState)
import Croq.Data.Id as Id
import Croq.Data.RouteType as RouteType exposing (RouteType)
import Croq.Data.Types exposing (..)
import Grades.Climbing as Climbing
import Json.Decode as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Maybe.Extra as Maybe


type alias Route =
    Climbable Id.ElemId Climbing.Grade RouteExtra


type alias RouteExtra =
    { kind : RouteType
    , completion : CompletionState
    , bolts : Maybe Bolts
    }


tags : Route -> List String
tags route =
    let
        completion =
            case route.extra.completion of
                CompletionState.IsProject ->
                    [ "project" ]

                CompletionState.IsUnfinished ->
                    [ "unfinished" ]

                _ ->
                    []
    in
    completion ++ Climbable.tags route


decoder : D.Decoder Route
decoder =
    let
        extra f x y z =
            f (RouteExtra x y z)
    in
    D.map extra (Climbable.decode Id.decodeElemId decodeGrade)
        |> D.optional "type" RouteType.decoder RouteType.Sport
        |> D.optional "completion" CompletionState.decoder CompletionState.IsFinished
        |> D.optional "bolts" (D.maybe NumBolts.decoder) Nothing


encoder : Route -> E.Value
encoder route =
    let
        extra =
            [ ( "type", RouteType.encoder route.extra.kind )
            , ( "completion", CompletionState.encoder route.extra.completion )
            , ( "bolts", Maybe.unwrap E.null NumBolts.encoder route.extra.bolts )
            ]
    in
    Climbable.encode Id.encodeElemId encodeGrade extra route


decodeGrade : D.Decoder Climbing.Grade
decodeGrade =
    D.string
        |> D.andThen
            (\s ->
                Climbing.parse s
                    |> Maybe.unwrap (D.fail "invalid grade") D.succeed
            )


encodeGrade : Climbing.Grade -> E.Value
encodeGrade grade =
    E.string (Climbing.show grade)
