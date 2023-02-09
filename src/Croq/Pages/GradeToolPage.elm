module Croq.Pages.GradeToolPage exposing (Model, Msg, init, update, view)

import Croq.Ui as Ui
import Croq.Ui as Ui
import Grades.Bouldering as Bouldering
import Grades.Climbing as Climbing
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)


type alias Model =
    { boulder : String, route : String }


type Msg
    = OnBoulderInput String
    | OnRouteInput String


init : Model
init =
    { boulder = "", route = "" }


update : Msg -> Model -> Model
update msg m =
    case msg of
        OnBoulderInput st ->
            { m | boulder = st }

        OnRouteInput st ->
            { m | route = st }


view : Model -> Html Msg
view m =
    Ui.appShell <|
        Ui.container
            [ Ui.title "Conversão de graus"
            , viewBoulder m.boulder
            , viewRoute m.route
            ]


viewBoulder : String -> Html Msg
viewBoulder st =
    let
        grade =
            Bouldering.parse st
    in
    div [ class "card p-4 shadow-lg card-bordered bg-secondary text-secondary-content" ]
        [ div [ class "card-body" ]
            [ h2 [ class "card-title" ] [ text "Boulder" ]
            , viewGradeInput
                { placeholder = "ex.: V3"
                , text = st
                , grade = grade
                , msg = OnBoulderInput
                , show = Bouldering.show
                , next = Bouldering.next
                , prev = Bouldering.prev
                }
            , viewConversions grade
                [ ( "Fontainebleau", Bouldering.showAs Bouldering.Fontainbleau )
                , ( "Escala V", Bouldering.showAs Bouldering.VGrade )
                , ( "Croq", Bouldering.toLinearScale >> String.fromFloat )
                ]
            ]
        ]


viewRoute : String -> Html Msg
viewRoute st =
    let
        grade =
            Climbing.parse st
    in
    div [ class "card p-4 my-8 shadow-lg card-bordered bg-primary text-primary-content" ]
        [ div [ class "card-body" ]
            [ h2 [ class "card-title" ] [ text "Vias" ]
            , viewGradeInput
                { placeholder = "ex.: 7a BR"
                , text = st
                , grade = grade
                , msg = OnRouteInput
                , show = Climbing.show
                , next = Climbing.next
                , prev = Climbing.prev
                }
            , viewConversions grade
                [ ( "US", Climbing.showAs Climbing.US )
                , ( "FR", Climbing.showAs Climbing.FR )
                , ( "BR", Climbing.showAs Climbing.BR )
                , ( "Croq", Climbing.toLinearScale >> String.fromFloat )
                ]
            ]
        ]


viewConversions : Maybe a -> List ( String, a -> String ) -> Html msg
viewConversions grade conversions =
    p [ class "prose mt-4" ]
        [ h3 [ class "h3" ] [ text "Conversões " ]
        , dl []
            (conversions
                |> List.map
                    (\( name, fn ) ->
                        [ dt [ class "font-bold" ] [ text name ]
                        , dd [ class "ml-4" ] [ text (grade |> Maybe.map fn |> withDefault "-") ]
                        ]
                    )
                |> List.concat
            )
        ]


viewGradeInput :
    { grade : Maybe b
    , show : b -> String
    , next : b -> b
    , prev : b -> b
    , text : String
    , placeholder : String
    , msg : String -> Msg
    }
    -> Html Msg
viewGradeInput cfg =
    let
        btn : Bool -> String -> Html Msg
        btn isNext label =
            let
                disabled =
                    case cfg.grade of
                        Just g ->
                            [ onClick (cfg.msg <| cfg.show <| select isNext cfg.next cfg.prev g) ]

                        _ ->
                            [ class "btn-disabled" ]
            in
            button (class "btn btn-primary btn-circle" :: disabled) [ text label ]
    in
    div [ class "form-control w-full" ]
        [ label [ class "label " ] [ span [] [ text "Digite o grau" ] ]
        , div [ class "flex space-x-2" ]
            [ btn False "-"
            , input
                [ class "input input-bordered w-full text-black"
                , if cfg.text == "" then
                    placeholder cfg.placeholder

                  else
                    value cfg.text
                , onInput cfg.msg
                ]
                []
            , btn True "+"
            ]
        ]


select : Bool -> c -> c -> c
select a b c =
    if a then
        b

    else
        c
