module Croq.Config exposing (..)

import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Grades.Bouldering
import Grades.Climbing
import Http exposing (Error(..))


type alias Model =
    { navKey : Key
    , api : String
    , static : String
    , translations : Dict String String
    , boulderingGrades : Grades.Bouldering.System
    , climbingGrades : Grades.Climbing.System
    }


type Msg
    = NoOp
    | OnSetBoulderingGrades Grades.Bouldering.System
    | OnSetClimbingGrades Grades.Climbing.System


init : String -> Key -> Model
init hostname key =
    case hostname of
        "croq-app.github.io" ->
            { navKey = key
            , api = "/frontend/api/"
            , static = "/frontend/static/"
            , translations = Dict.empty
            , boulderingGrades = Grades.Bouldering.VGrade
            , climbingGrades = Grades.Climbing.BR
            }

        _ ->
            { navKey = key
            , api = "/api/"
            , static = "/static/"
            , translations = Dict.empty
            , boulderingGrades = Grades.Bouldering.VGrade
            , climbingGrades = Grades.Climbing.BR
            }


translate : Model -> String -> String
translate m st =
    Dict.get st m.translations
        |> Maybe.withDefault st


update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ m =
    let
        return m_ =
            ( m_, Cmd.none )
    in
    case msg_ of
        NoOp ->
            return m

        OnSetBoulderingGrades sys ->
            return { m | boulderingGrades = sys }

        OnSetClimbingGrades sys ->
            return { m | climbingGrades = sys }


showBoulderingGrade : Model -> Grades.Bouldering.Grade -> String
showBoulderingGrade cfg grade =
    Grades.Bouldering.showAs cfg.boulderingGrades grade


showClimbingGrade : Model -> Grades.Climbing.Grade -> String
showClimbingGrade cfg grade =
    Grades.Climbing.showAs cfg.climbingGrades grade


pushUrl : String -> Model -> Cmd msg
pushUrl url m =
    Nav.pushUrl m.navKey url


pushErrorUrl : Error -> Model -> Cmd msg
pushErrorUrl e m =
    Nav.pushUrl m.navKey ("/error?msg=" ++ errorMsg e)


errorMsg : Error -> String
errorMsg e =
    case e of
        BadUrl url ->
            "bad url: " ++ url

        Timeout ->
            "operation timed out"

        NetworkError ->
            "network error"

        BadStatus i ->
            "bad status: " ++ String.fromInt i

        BadBody st ->
            "bad body: " ++ st
