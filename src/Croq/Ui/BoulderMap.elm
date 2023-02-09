module Croq.Ui.BoulderMap exposing (..)

{-| A topdown vision of a Map with some highlighted boulders. Follows TEA.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as SA
import Croq.Ui.Color exposing (..)


type alias Model =
    { selected : Maybe String }


type Msg
    = OnSelectBlock String


view : Model -> Html Msg
view m =
    div [ class "bg-gradient-to-r from-cyan-500 to-blue-500 my-4 h-48 rounded-md" ]
        [ text "TODO: Topdown view of boulder formations in a sector"

        -- , img [ class "w-full", src "/static/boulder-sector.svg" ] []
        , example (m.selected |> Maybe.withDefault "")
        ]


update : Msg -> Model -> Model
update msg m =
    case msg of
        OnSelectBlock id ->
            { m
                | selected =
                    if m.selected == Just id then
                        Nothing

                    else
                        Just id
            }


init : Model
init =
    { selected = Nothing }


example : String -> Html Msg
example selected =
    let
        selectClass cls =
            if cls == selected then
                SA.class "selected"

            else
                SA.class "unselected"
    in
    S.svg
        [ SA.width "300"
        , SA.height "200"
        , SA.viewBox "0 0 321 228"
        , SA.fill "none"
        , SA.class "w-full"
        ]
        [ S.style []
            [ text """
                .formation { fill: #F1CB66; fill-opacity: 0.6; } 
                .formation:hover { fill-opacity: 1.0; stroke: black; }
                .formation.selected { fill: red }
                .formation.unselected {  }
            """ ]
        , S.rect
            [ SA.x "0.5"
            , SA.y "0.5"
            , SA.width "320"
            , SA.height "227"
            , SA.rx "3.5"
            , SA.fill "#f0ede6"
            , SA.fillOpacity "0.9"
            , SA.stroke "#84B776"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "a"
            , onClick (OnSelectBlock "a")
            , SA.d "M114.078 156.333C106 148.061 97.2478 157.482 93.8817 163.227C90.5157 173.567 97.921 175.635 87.1494 183.907C73.6849 194.247 100.614 197.693 114.078 183.907C127.543 170.12 124.177 166.673 114.078 156.333Z"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "b"
            , onClick (OnSelectBlock "b")
            , SA.d "M56.9847 149.481C64.2891 163.673 79.5777 154.875 86.309 148.703C94.884 136.486 85.9758 130.729 103.459 124.271C125.314 116.198 91.2851 100.617 68.0479 113.222C44.8107 125.827 47.8541 131.74 56.9847 149.481Z"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "c"
            , onClick (OnSelectBlock "c")
            , SA.d "M183.136 63.1908C181.81 47.2476 164.349 49.4006 155.784 52.47C143.213 60.3959 149.235 69.1829 130.641 68.3214C107.399 67.2444 132.851 94.8933 159.098 92.328C185.345 89.7628 184.793 83.1197 183.136 63.1908Z"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "d"
            , onClick (OnSelectBlock "d")
            , SA.d "M243.584 107.533C229.909 90.3608 211.637 106.138 204.21 116.173C195.92 134.803 209.492 140.23 187.629 153.433C160.299 169.937 210.42 182.053 238.398 159.103C266.376 136.153 260.678 128.998 243.584 107.533Z"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "e"
            , onClick (OnSelectBlock "e")
            , SA.d "M276.776 30.6162C276.318 25.1064 270.283 25.8505 267.323 26.9112C262.979 29.6503 265.06 32.687 258.634 32.3893C250.602 32.0171 259.398 41.5722 268.468 40.6857C277.539 39.7992 277.348 37.5034 276.776 30.6162Z"
            ]
            []
        , S.path
            [ SA.class "formation"
            , selectClass "f"
            , onClick (OnSelectBlock "f")
            , SA.d "M146.055 46.666C143.948 57.5076 135.64 58.8123 131.749 58.1095L125.267 55.9371C120.085 52.9971 112.582 44.8282 124.026 35.6734C138.331 24.2299 143.794 30.0489 138.578 39.1265C133.363 48.204 148.687 33.1141 146.055 46.666Z"
            ]
            []
        ]
