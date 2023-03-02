module Croq.Ui.Climbable exposing (fullTitle, sections)

import Croq.Data.Climbable exposing (Climbable)
import Croq.Data.ClimbingTechnique as ClimbingTechnique
import Croq.Data.Types exposing (viewOptionalRichText)
import Croq.Data.Warning as Warning
import Daisy.Elements as Ui
import Html exposing (a, text)
import Html.Attributes exposing (class)
import LatLng
import Maybe.Extra as Maybe


fullTitle : (grade -> String) -> Climbable id grade extra -> String
fullTitle showGrade data =
    data.name ++ " (" ++ Maybe.unwrap "∅" showGrade data.grade ++ ")"


sections : Climbable id grade extra -> List ( String, List (Html.Html msg) )
sections data =
    let
        opt : (a -> b) -> Maybe a -> List b
        opt f m =
            case m of
                Just x ->
                    [ f x ]

                _ ->
                    []

        list : (a -> Html.Html msg) -> List a -> List (Html.Html msg)
        list f xs =
            if xs == [] then
                []

            else
                [ Ui.list f [ class "list-disc pl-5" ] xs ]

        content =
            text
    in
    [ ( "Descrição", [ viewOptionalRichText "Sem descrição 😥" data.description ] )
    , ( "Coordenadas", opt locationSection data.location )
    , ( "Altura", opt (\h -> content <| String.fromInt h ++ "m") data.height )
    , ( "Ano de abertura", opt (content << String.fromInt) data.year )
    , ( "Primeira ascensão", opt content data.firstAscent )
    , ( "Aberto por", opt content data.firstAscent )
    , ( "Conquistadores", list content data.establishedBy )
    , ( "Técnicas", list (ClimbingTechnique.toString >> content) data.techniques )
    , ( "Avisos", list (Warning.toString >> content) data.warnings )
    , ( "Vídeos", list (\url -> a (Ui.link url) [ content url ]) data.videos )
    , ( "Betas", list content data.betas )
    ]


locationSection : LatLng.LatLng -> Html.Html msg
locationSection latlng =
    a (Ui.link <| LatLng.googleMapsUrl latlng) [ text <| LatLng.toString latlng ]
