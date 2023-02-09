module Croq.Ui.Histogram exposing (..)

{-| An Histogram of climbing grades (or anything else). Follows TEA.
-}

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Croq.Ui.Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra


type alias Model =
    List (CI.One Datum CI.Bar)


type Msg
    = OnEvents Event Model


type Event
    = Click
    | Hover


{-| Get label of the clicked bar from message
-}
getClicked : Msg -> Maybe String
getClicked (OnEvents ev lst) =
    case ( ev, lst ) of
        ( Click, [ x ] ) ->
            Just <| Tuple.first (CI.getData x)

        _ ->
            Nothing


type alias Datum =
    ( String, Float )


init : Model
init =
    []


view : Model -> List Datum -> Html Msg
view hover data =
    div [ class "w-96 h-48 mt-8 mb-12 mx-auto" ]
        [ C.chart
            [ CA.height 240
            , CA.width 480
            , CE.onMouseMove (OnEvents Hover) (CE.getNearest CI.bars)
            , CE.onMouseLeave (OnEvents Hover [])
            , CE.onClick (OnEvents Click) (CE.getNearest CI.bars)
            ]
            [ C.xLabels
                [ CA.rotate 90
                , CA.ints
                , CA.amount (List.length data)
                , CA.moveRight 5
                , CA.moveDown 5
                , CA.format (\x -> List.Extra.getAt (round (x - 1)) data |> Maybe.map Tuple.first |> Maybe.withDefault "?")
                ]
            , C.barLabels [ CA.moveUp 5 ]
            , C.bars [ CA.roundBottom 0.5, CA.roundTop 0.5, CA.margin 0.1 ]
                [ C.bar Tuple.second [ CA.gradient [ "#529B03", CA.green ] ]
                    |> C.amongst hover (\_ -> [ CA.highlight 0.25 ])
                    |> C.named "n"
                ]
                data
            , C.each hover <|
                \_ item ->
                    [ C.tooltip item [] [] [] ]
            ]
        ]


update : Msg -> Model -> Model
update msg _ =
    case msg of
        OnEvents _ new ->
            new
