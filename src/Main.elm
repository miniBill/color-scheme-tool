module Main exposing (Model, Msg, main)

import Brewer
import Browser
import Color
import Color.Oklch as Oklch exposing (Oklch)
import FathersDay
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Round
import Theme


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
    Theme.column
        [ Html.Attributes.style "padding" "8px" ]
        [ Theme.wrappedRow
            []
            [ viewSlice l h model
            , viewSlice c h model
            , viewSlice l c model
            ]
        , viewPalettes model
        ]


type alias Component =
    { get : Oklch -> Float
    , set : Float -> Oklch -> Oklch
    , max : Float
    }


l : Component
l =
    { get = .lightness
    , set = \new color -> { color | lightness = new }
    , max = 1
    }


c : Component
c =
    { get = .chroma
    , set = \new color -> { color | chroma = new }
    , max = 0.37
    }


h : Component
h =
    { get = .hue
    , set = \new color -> { color | hue = new }
    , max = 1
    }


viewSlice : Component -> Component -> Palette -> Html Msg
viewSlice xComponent yComponent palette =
    Theme.box [] [ Html.text "TODO - viewSlice" ]


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
        |> Theme.wrappedRow [ Html.Attributes.style "align-items" "start" ]


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
            , Html.Events.onClick colors
            ]

        selectionAttrs : List (Attribute Msg)
        selectionAttrs =
            if selected then
                [ Html.Attributes.style "box-shadow" "0px 0px 4px 4px #ccf"
                , Html.Attributes.style "background" "#f0f0ff"
                , gridTemplate Column
                    [ ( [ "color" ], "24px" )
                    , ( [], "8px" )
                    , ( [ "oklch" ], "auto" )
                    , ( [ "l" ], "auto" )
                    , ( [], "8px" )
                    , ( [ "c" ], "auto" )
                    , ( [], "8px" )
                    , ( [ "h" ], "auto" )
                    , ( [], "16px" )
                    , ( [ "rgb" ], "auto" )
                    , ( [ "r" ], "auto" )
                    , ( [], "8px" )
                    , ( [ "g" ], "auto" )
                    , ( [], "8px" )
                    , ( [ "b" ], "auto" )
                    ]
                ]

            else
                [ gridTemplate Column
                    [ ( [ "color" ], "24px" )
                    ]
                ]
    in
    colors
        |> List.concatMap (viewColor { selected = selected })
        |> Theme.box attrs


type GridAxis
    = Row
    | Column


gridTemplate : GridAxis -> List ( List String, String ) -> Attribute msg
gridTemplate axis others =
    let
        axisString : String
        axisString =
            case axis of
                Row ->
                    "rows"

                Column ->
                    "columns"

        pieces : List String
        pieces =
            List.concatMap otherToString others

        labelsToString : List String -> List String
        labelsToString labels =
            if List.isEmpty labels then
                []

            else
                [ "[" ++ String.join " " labels ++ "]" ]

        otherToString : ( List String, String ) -> List String
        otherToString ( labels, column ) =
            labelsToString labels
                ++ (if String.isEmpty column then
                        []

                    else
                        [ column ]
                   )
    in
    Html.Attributes.style ("grid-template-" ++ axisString) (String.join " " pieces)


viewColor : { selected : Bool } -> Oklch -> List (Html Msg)
viewColor { selected } color =
    let
        colorDiv : Html msg
        colorDiv =
            Html.div
                [ Html.Attributes.style "background-color" (Oklch.toCssString color)
                , Html.Attributes.style "grid-column" "color"
                , Html.Attributes.style "width" "24px"
                , Html.Attributes.style "height" "24px"
                ]
                []
    in
    if selected then
        let
            rgb : { red : Float, green : Float, blue : Float, alpha : Float }
            rgb =
                color
                    |> Oklch.toColor
                    |> Color.toRgba
        in
        [ colorDiv
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
        , Html.span
            [ Html.Attributes.style "grid-column" "rgb" ]
            [ Html.text "rgb(" ]
        , Html.span
            [ Html.Attributes.style "grid-column" "r"
            , Html.Attributes.style "justify-self" "right"
            ]
            [ Html.text (Round.round 0 (rgb.red * 100) ++ "% ") ]
        , Html.span
            [ Html.Attributes.style "grid-column" "g"
            , Html.Attributes.style "justify-self" "right"
            ]
            [ Html.text (Round.round 0 (rgb.green * 100) ++ "% ") ]
        , Html.span
            [ Html.Attributes.style "grid-column" "b"
            , Html.Attributes.style "justify-self" "right"
            ]
            [ Html.text (Round.round 0 (rgb.blue * 100) ++ "% ") ]
        ]

    else
        [ colorDiv ]


update : Msg -> Model -> Model
update msg _ =
    msg
