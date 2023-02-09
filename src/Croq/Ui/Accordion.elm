module Croq.Ui.Accordion exposing (..)

{-| Accordion. Follows TEA.
-}

import Croq.Ui as Ui
import Croq.Ui.Color as Color
import Croq.Util exposing (iff)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias State =
    { selected : Int }


type alias Config data msg =
    { toMsg : State -> msg
    , render : data -> Html msg
    , title : data -> String
    }


view : Config data msg -> State -> List data -> Html msg
view cfg m data =
    let
        item i elem =
            let
                viewHandle h =
                    span [ class "align-right font-bold text-2xl" ] [ text (" " ++ h) ]

                handle =
                    viewHandle <| iff (i == m.selected) "-" "+"

                title =
                    span [ class "px-3 flex-1 text-lg font-bold" ] [ text (cfg.title elem) ]

                expand =
                    if i == m.selected then
                        div [ class "ml-8" ] [ cfg.render elem ]

                    else
                        text ""

                msg =
                    if i == m.selected then
                        cfg.toMsg { selected = -1 }

                    else
                        cfg.toMsg { selected = i }
            in
            div
                [ class "px-4 py-2 border-t first:border-t-0"
                , class "text-left text-sm"
                , class "focus:bg-slate-100 hover:outline-none hover:ring hover:ring primary-focus"
                , onClick msg
                ]
                [ div [ class "block flex items-center h-14" ] [ Ui.counter Color.Primary (i + 1), title, handle ]
                , expand
                ]
    in
    div [ class "rounded-md shadow-xl" ] (List.indexedMap item data)


init : State
init =
    { selected = -1 }


config : Config data msg -> Config data msg
config cfg =
    cfg


actionButton : List (Attribute msg) -> List (Html msg) -> Html msg
actionButton attrs body =
    div [ class "my-4 text-primary font-bold text-[1.0625rem]" ]
        [ a (class "" :: attrs) body
        , span [ class "px-2" ] [ text " →" ]
        ]
