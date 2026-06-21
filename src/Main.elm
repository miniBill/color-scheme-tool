module Main exposing (Model, Msg, main)

import Brewer
import Browser
import Color exposing (Color)
import Color.Oklch as Oklch exposing (Oklch)
import Html exposing (Html)
import Html.Attributes
import Round


type alias Model =
    List Oklch


type alias Msg =
    Model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    Brewer.set3 |> List.map Oklch.fromColor


view : Model -> Html Msg
view colors =
    Html.div
        [ Html.Attributes.style "padding" "8px" ]
        [ colors
            |> List.concatMap viewColor
            |> Html.div
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "gap" "8px 0"
                , Html.Attributes.style "grid-template-columns" "[color] 24px 8px [oklch] auto [l] auto 8px [c] auto 8px [h] auto 1fr"
                ]
        ]


viewColor : Oklch -> List (Html Msg)
viewColor color =
    [ Html.span
        [ Html.Attributes.style "background-color" (Oklch.toCssString color)
        , Html.Attributes.style "grid-column" "color"
        ]
        []
    , Html.span
        [ Html.Attributes.style "grid-column" "oklch" ]
        [ Html.text "oklch(" ]
    , Html.span
        [ Html.Attributes.style "grid-column" "l"
        , Html.Attributes.style "justify-self" "right"
        ]
        [ Html.text (Round.round 0 (color.lightness * 100) ++ "% ") ]
    , Html.span
        [ Html.Attributes.style "grid-column" "c"
        , Html.Attributes.style "justify-self" "right"
        ]
        [ Html.text (Round.round 3 color.chroma ++ " ") ]
    , Html.span
        [ Html.Attributes.style "grid-column" "h"
        , Html.Attributes.style "justify-self" "right"
        ]
        [ Html.text (Round.round 0 (color.hue * 360) ++ ")") ]
    , Html.span [] []
    ]


update : Msg -> Model -> Model
update msg _ =
    msg
