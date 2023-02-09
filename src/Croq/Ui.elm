module Croq.Ui exposing (..)

import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Types exposing (..)
import Croq.Routes as Routes
import Croq.Ui.Color exposing (Color, colorString, fullColor)
import Croq.Ui.Svg as Icons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
        , attribute "data-theme" "lemonade"
        ]
        [ navbar
        , main_ [] [ content ]
        ]


navbar : Html msg
navbar =
    let
        icon svg =
            button [ class "btn btn-square btn-ghost" ] [ svg ]
    in
    div
        [ class "navbar shadow-lg bg-primary primary-content"
        ]
        [ div [ class "flex-none" ] [ icon Icons.menu ]
        , div
            [ class "flex-1" ]
            [ a
                [ class "btn btn-ghost normal-case text-xl"
                , class "hover:text-white"
                , href Routes.homeUrl
                ]
                [ text "croq.app" ]
            ]
        , div [ class "flex-none" ] [ icon Icons.dots ]
        ]


{-| A simple button
-}
btn : List (Attribute msg) -> List (Html msg) -> Html msg
btn attrs body =
    button (attrs ++ []) body


list : List (Attribute msg) -> (a -> Html msg) -> List a -> Html msg
list attrs func items =
    ul (class "list-disc pl-4":: attrs)
        (List.map
            (func >> List.singleton >> li [])
            items
        )


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
            (li [] [ a [ href Routes.homeUrl ] [ Icons.home ] ]
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


tag : String -> Html msg
tag txt =
    span [ class "badge badge-outline badge-primary" ] [ text txt ]


tags : List String -> Html msg
tags xs =
    div [ class "space-x-1" ] (List.map tag xs)


container : List (Html msg) -> Html msg
container content =
    div [ class "mx-auto px-4 my-2 max-w-lg" ] content


title : String -> Html msg
title txt =
    h1 [ class "font-bold text-xl mb-2" ] [ text txt ]


link : Url -> List (Attribute msg)
link url =
    if String.startsWith "/" url then
        [ href url ]

    else
        [ href url, target "blank_" ]


sections : List ( String, List (Html msg) ) -> Html msg
sections data =
    let
        item ( st, children ) =
            [ dt [ class "text-lg font-bold my-2" ] [ text st ]
            , dd [ class "mb-4" ] children
            ]
    in
    dl []
        (data
            |> List.filter (Tuple.second >> (/=) [])
            |> List.map item
            |> List.concat
        )


urlList : String -> List (Attribute msg) -> List ( Url, String ) -> Html msg
urlList empty attrs items =
    case items of
        [] ->
            text empty

        _ ->
            ul attrs (List.map (\( url, txt ) -> li [] [ a (link url) [ text txt ] ]) items)


shortPlaceholder : String
shortPlaceholder =
    "░░░░░ ░ ░░░░"


longPlaceholder : String
longPlaceholder =
    "░░░░░ ░ ░░░░ ░░░░ ░░░ ░░ ░░░░░ ░ ░░░░ ░ ░░░░ ░░░ ░░░ ░░░░ ░░░ ░░ ░░░░░ ░ ░░░░ ░ ░░░░"


loremIpsum : String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
