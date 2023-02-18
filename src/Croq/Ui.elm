module Croq.Ui exposing (..)

import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Types exposing (..)
import Croq.Routes as Routes
import Croq.Ui.Color exposing (Color, colorString, fullColor)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.Icons as I
import Material.Icons.Round as IR
import Material.Icons.Types as I
import Maybe.Extra as Maybe


type alias CardListItem msg =
    ( String, List (Attribute msg) )


viewLoading : LoadingHttp a -> (a -> Html msg) -> Html msg
viewLoading =
    Loading.view


viewOptional : (a -> Html msg) -> Maybe a -> Html msg
viewOptional =
    Maybe.unwrap (text "")



--- COMPONENTS


appShell : Html msg -> Html msg
appShell content =
    div
        [ class "bg-base w-100"
        , attribute "data-theme" "croq"
        ]
        [ navbar
        , main_ [] [ content ]
        ]


navbar : Html msg
navbar =
    let
        icon i =
            button [ class "btn btn-square btn-ghost" ] [ i 24 I.Inherit ]
    in
    div
        [ class "navbar shadow-lg bg-primary primary-content"
        ]
        [ div [ class "flex-none" ] [ icon I.menu ]
        , div
            [ class "flex-1" ]
            [ a
                [ class "btn btn-ghost normal-case text-xl"
                , class "hover:text-white"
                , href Routes.homeUrl
                ]
                [ text "croq.app" ]
            ]
        , div [ class "flex-none" ] [ icon I.more_vert ]
        ]


actionBtn : List (Attribute msg) -> List (Html msg) -> Html msg
actionBtn attrs body =
    div [ class "my-4 text-primary font-bold text-[1.0625rem]" ]
        [ a (class "" :: attrs) body
        , span [ class "px-2" ] [ text " →" ]
        ]


{-| A simple rounded counter with specific color
-}
counter : Color -> Int -> Html msg
counter color i =
    div
        [ class "h-5 w-5 font-bold text-sm text-center rounded-full shadow-sm"
        , fullColor color
        ]
        [ text (String.fromInt i) ]


{-|

    Renders a list of pairs of (title, [attrs]) as cards.

    Each card triggers the corresponding message onClick events.

-}
cardList : HtmlElem msg -> Color -> List (CardListItem msg) -> Html msg
cardList elem color items =
    let
        viewItem i ( name, attrs ) =
            card i color elem attrs [ text name ]
    in
    div [ class "grid grid-cols-2 gap-1" ] (List.indexedMap viewItem items)


card : Int -> Color -> (List (Attribute msg) -> List (Html msg) -> Html msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
card i color elem attrs content =
    elem
        ([ class "block flex items-center px-4 py-2 h-14 rounded-md shadow-md"
         , class "text-left text-sm"
         , class ("focus:bg-slate-100 hover:outline-none hover:ring hover:ring-" ++ colorString color ++ "-focus")
         ]
            ++ attrs
        )
        [ counter color (i + 1)
        , span [ class "flex-1 mx-3" ] content
        ]


breadcrumbs : List ( Url, Name ) -> Html msg
breadcrumbs links =
    div [ class "text-sm breadcrumbs" ]
        [ ul [] <|
            (li [] [ a [ href Routes.homeUrl ] [ IR.home 16 I.Inherit ] ]
                :: List.map
                    (\( url, name ) ->
                        if url == "" then
                            li [] [ text name ]

                        else
                            li []
                                [ a
                                    [ href url
                                    , class "text-primary font-bold"
                                    ]
                                    [ text name ]
                                ]
                    )
                    (skipLastBreadcrumb links)
            )
        ]


skipLastBreadcrumb : List ( Url, Name ) -> List ( Url, Name )
skipLastBreadcrumb lst =
    case lst of
        [] ->
            []

        [ ( _, name ) ] ->
            [ ( "", name ) ]

        p :: ps ->
            p :: skipLastBreadcrumb ps
