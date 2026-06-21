module Main exposing (Model, Msg, main)

import Brewer
import Browser
import Color.Oklch as Oklch exposing (Oklch)
import FathersDay
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Round


type alias Palette =
    List Oklch


type alias Model =
    Palette


type alias Msg =
    Palette


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
view model =
    Html.div
        [ Html.Attributes.style "padding" "8px" ]
        [ viewPalettes model
        ]


viewPalettes : Model -> Html Msg
viewPalettes model =
    let
        all : List Palette
        all =
            (FathersDay.palette :: Brewer.all)
                |> List.sortBy List.length
                |> List.map (List.map Oklch.fromColor)
    in
    all
        |> List.map
            (\palette ->
                viewPalette { selected = model == palette } palette
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "align-items" "start"
            ]


viewPalette : { selected : Bool } -> Palette -> Html Msg
viewPalette { selected } colors =
    let
        attrs : List (Attribute Msg)
        attrs =
            commonAttrs ++ selectionAttrs

        commonAttrs : List (Attribute Msg)
        commonAttrs =
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "gap" "8px 0"
            , Html.Attributes.style "grid-template-columns" "[color] 24px 8px [oklch] auto [l] auto 8px [c] auto 8px [h] auto"
            , Html.Events.onClick colors
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "border-radius" "8px"
            ]

        selectionAttrs : List (Attribute Msg)
        selectionAttrs =
            if selected then
                [ Html.Attributes.style "box-shadow"
                    "0px 0px 4px 4px #ccf"
                , Html.Attributes.style "background"
                    "#f0f0ff"
                ]

            else
                [ Html.Attributes.style "box-shadow"
                    "initial"
                , Html.Attributes.style "background"
                    "initial"
                ]
    in
    colors
        |> List.concatMap viewColor
        |> Html.div attrs


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
