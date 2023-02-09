module Croq.Ui.Color exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


type Color
    = Primary
    | Secondary
    | Accent
    | Neutral
    | Info
    | Success
    | Warning
    | Error
    | Base
    | Invert


colorString : Color -> String
colorString color =
    case color of
        Primary ->
            "primary"

        Secondary ->
            "secondary"

        Accent ->
            "accent"

        Neutral ->
            "neutral"

        Info ->
            "info"

        Success ->
            "success"

        Warning ->
            "warning"

        Error ->
            "error"

        Base ->
            "base-100"

        Invert ->
            "base-content"


bgColor : Color -> Attribute msg
bgColor color =
    class <|
        case color of
            Primary ->
                "bg-primary"

            Secondary ->
                "bg-secondary"

            Accent ->
                "bg-accent"

            Neutral ->
                "bg-neutral"

            Info ->
                "bg-info"

            Success ->
                "bg-success"

            Warning ->
                "bg-warning"

            Error ->
                "bg-error"

            Base ->
                "bg-base-100"

            Invert ->
                "bg-base-content"


fullColor : Color -> Attribute msg
fullColor color =
    class <|
        case color of
            Primary ->
                "bg-primary text-primary-content"

            Secondary ->
                "bg-secondary text-secondary-content"

            Accent ->
                "bg-accent text-accent-content"

            Neutral ->
                "bg-neutral text-neutral-content"

            Info ->
                "bg-info text-info-content"

            Success ->
                "bg-success text-success-content"

            Warning ->
                "bg-warning text-warning-content"

            Error ->
                "bg-error text-error-content"

            Base ->
                "bg-base-100 text-base-content"

            Invert ->
                "bg-base-content text-base-100"
