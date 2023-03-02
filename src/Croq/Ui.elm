module Croq.Ui exposing (..)

import Croq.Api as Api
import Croq.Config as Cfg
import Croq.Data.Loading as Loading exposing (LoadingHttp)
import Croq.Data.Types exposing (..)
import Croq.Routes as Routes
import Croq.Ui.Color exposing (Color, colorString, fullColor)
import Daisy.Elements as Ui
import Grades.Bouldering
import Grades.Climbing
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


appShell : Cfg.Model -> (Cfg.Msg -> msg) -> (msgContent -> msg) -> Html msgContent -> Html msg
appShell cfg onCfg onContent content =
    div [ class "bg-base w-100", attribute "data-theme" "croq" ]
        [ navbar
        , main_
            [ class "drawer drawer-mobile content-height overflow-y" ]
            [ input [ id "app-drawer", type_ "checkbox", class "drawer-toggle" ] []
            , div [ class "drawer-content" ]
                [ Html.map onContent content ]
            , div [ class "drawer-side" ]
                [ label [ for "app-drawer", class "drawer-overlay" ] []
                , Html.map onCfg (drawer cfg)
                ]
            ]
        ]


drawer : Cfg.Model -> Html Cfg.Msg
drawer cfg =
    div [ class "p-4 w-72 lg:w-80 bg-base-100 lg:bg-base-200 text-base-content flex flex-col content-height fixed" ]
        [ div [ class "flex flex-col" ]
            [ label [ class "py-2" ] [ text "Grau (Via)" ]
            , routeGradeSystemSelector cfg
            ]
        , div [ class "flex flex-col mt-4" ]
            [ label [ class "py-2" ] [ text "Grau (Boulder)" ]
            , boulderGradeSystemSelector cfg
            ]
        , div [ class "flex-1" ] []
        , a (class "block mb-4 mx-auto w-12" :: Ui.link "http://github.com/croq-app/frontend") [ img [ alt "github.com", src (Api.static cfg "img/github.svg") ] [] ]
        ]


routeGradeSystemSelector : Cfg.Model -> Html Cfg.Msg
routeGradeSystemSelector cfg =
    let
        select sys =
            if sys == cfg.climbingGrades then
                [ class "btn-active", onClick (Cfg.OnSetClimbingGrades sys) ]

            else
                [ onClick (Cfg.OnSetClimbingGrades sys) ]
    in
    div [ class "btn-group w-full" ]
        [ div (class "btn w-16 btn-sm" :: select Grades.Climbing.BR) [ text "BR" ]
        , div (class "btn w-16 btn-sm" :: select Grades.Climbing.FR) [ text "FR" ]
        , div (class "btn w-16 btn-sm" :: select Grades.Climbing.US) [ text "US" ]
        ]


boulderGradeSystemSelector : Cfg.Model -> Html Cfg.Msg
boulderGradeSystemSelector cfg =
    let
        select sys =
            if sys == cfg.boulderingGrades then
                [ class "btn-active", onClick (Cfg.OnSetBoulderingGrades sys) ]

            else
                [ onClick (Cfg.OnSetBoulderingGrades sys) ]
    in
    div [ class "btn-group w-full" ]
        [ div (class "btn btn-sm" :: select Grades.Bouldering.VGrade) [ text "Hueco" ]
        , div (class "btn btn-sm" :: select Grades.Bouldering.Fontainbleau) [ text "Fontainbleau" ]
        ]


navbar : Html msg
navbar =
    let
        icon attrs i =
            button (class "btn btn-square btn-ghost" :: attrs) [ i 24 I.Inherit ]
    in
    div
        [ class "navbar shadow-lg bg-primary primary-content"
        ]
        [ div [ class "flex-none lg:hidden" ] [ label [ for "app-drawer", class "drawer-button btn btn-square btn-ghost" ] [ I.menu 25 I.Inherit ] ]
        , div
            [ class "flex-1" ]
            [ a
                [ class "btn btn-ghost normal-case text-xl"
                , class "hover:text-white"
                , href Routes.homeUrl
                ]
                [ text "croq.app" ]
            ]
        , div [ class "flex-none" ] [ icon [] I.search ]
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


classes : String
classes =
    """
    badge badge-outline badge-sm

    space-x-1

    pl-4

    list-disc
    """
