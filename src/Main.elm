module Main exposing (Model, Msg, main)

import Brewer
import Browser
import Color
import Color.Oklch as Oklch exposing (Oklch)
import FathersDay
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import Round
import Svg exposing (Svg)
import Svg.Attributes
import Theme
import WebGL exposing (Mesh, Shader)


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
        [ Theme.padding ]
        [ Theme.wrappedRow []
            [ viewSlice l c h model
            , viewSlice h c l model
            , viewSlice h l c model
            ]
        , viewPalettes model
        ]


type alias Component =
    { get : Oklch -> Float
    , set : Float -> Oklch -> Oklch
    , max : Float
    , default : Float
    , component : Vec3
    , label : String
    }


l : Component
l =
    { get = .lightness
    , set = \new color -> { color | lightness = new }
    , max = 1
    , default = 0.7
    , component = Vector3.vec3 1 0 0
    , label = "L"
    }


c : Component
c =
    { get = .chroma
    , set = \new color -> { color | chroma = new }
    , max = 0.37
    , default = 0.1
    , component = Vector3.vec3 0 1 0
    , label = "C"
    }


h : Component
h =
    { get = .hue
    , set = \new color -> { color | hue = new }
    , max = 1
    , default = 0
    , component = Vector3.vec3 0 0 1
    , label = "H"
    }


viewSlice : Component -> Component -> Component -> Palette -> Html Msg
viewSlice xComponent yComponent missingComponent palette =
    let
        padding : number
        padding =
            25

        innerWidth : number
        innerWidth =
            380

        outerWidth : number
        outerWidth =
            innerWidth + 2 * padding

        innerHeight : number
        innerHeight =
            200

        outerHeight : number
        outerHeight =
            innerHeight + 2 * padding

        axes : List (Svg msg)
        axes =
            [ Svg.line
                [ Svg.Attributes.x1 "-10"
                , Svg.Attributes.x2 (String.fromFloat (innerWidth + 10))
                , Svg.Attributes.stroke "white"
                , Svg.Attributes.y1 (String.fromFloat innerHeight)
                , Svg.Attributes.y2 (String.fromFloat innerHeight)
                , Svg.Attributes.strokeWidth "2"
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x (String.fromInt (innerWidth - 5))
                , Svg.Attributes.y (String.fromInt (innerHeight + 5))
                , Svg.Attributes.fill "white"
                , Svg.Attributes.dominantBaseline "hanging"
                ]
                [ Svg.text xComponent.label ]
            , Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.x2 "0"
                , Svg.Attributes.stroke "white"
                , Svg.Attributes.y1 "-10"
                , Svg.Attributes.y2 (String.fromInt (innerHeight + 10))
                , Svg.Attributes.strokeWidth "2"
                ]
                []
            , Svg.text_
                [ Svg.Attributes.x "-15"
                , Svg.Attributes.y "5"
                , Svg.Attributes.fill "white"
                ]
                [ Svg.text yComponent.label ]
            ]

        dots : List (Svg msg)
        dots =
            palette
                |> List.map
                    (\color ->
                        Svg.circle
                            [ Svg.Attributes.cx
                                (xComponent.get color
                                    |> project 0 xComponent.max 0 innerWidth
                                    |> String.fromFloat
                                )
                            , Svg.Attributes.cy
                                (yComponent.get color
                                    |> project 0 yComponent.max innerHeight 0
                                    |> String.fromFloat
                                )
                            , Svg.Attributes.r "5"
                            , color
                                |> Oklch.toCssString
                                |> Svg.Attributes.fill
                            , Svg.Attributes.stroke "black"
                            ]
                            []
                    )

        svg : Html msg
        svg =
            Svg.svg
                [ Html.Attributes.width outerWidth
                , [ -padding, -padding, outerWidth, outerHeight ]
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , Html.Attributes.style "display" "block"
                , Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "top" "8px"
                , Html.Attributes.style "left" "8px"
                ]
                (axes ++ dots)

        missingComponentAverage : Float
        missingComponentAverage =
            if List.isEmpty palette then
                missingComponent.default

            else
                (palette
                    |> List.map missingComponent.get
                    |> List.sum
                )
                    / toFloat (List.length palette)

        webgl : Html msg
        webgl =
            WebGL.toHtml
                [ Html.Attributes.width outerWidth
                , Html.Attributes.height outerHeight
                , Html.Attributes.style "display" "block"
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    mesh
                    { componentMatrix =
                        Matrix4.makeBasis
                            (Vector3.scale xComponent.max xComponent.component)
                            (Vector3.scale yComponent.max yComponent.component)
                            (Vector3.scale missingComponentAverage missingComponent.component)
                    , innerHeight = innerHeight
                    , innerWidth = innerWidth
                    , padding = padding
                    }
                ]
    in
    Theme.box
        [ Html.Attributes.style "position" "relative" ]
        [ webgl
        , svg
        ]


type alias Vertex =
    { position : Vec2
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangleFan
        [ Vertex (Math.Vector2.vec2 -1 -1)
        , Vertex (Math.Vector2.vec2 -1 1)
        , Vertex (Math.Vector2.vec2 1 1)
        , Vertex (Math.Vector2.vec2 1 -1)
        ]


type alias Uniforms =
    { innerWidth : Float
    , innerHeight : Float
    , padding : Float
    , componentMatrix : Mat4
    }


vertexShader : Shader Vertex Uniforms { vpos : Vec2 }
vertexShader =
    [glsl|
        attribute vec2 position;
        varying vec2 vpos;

        void main () {
            gl_Position = vec4(position, 0, 1);
            vpos = position;
        }
    |]


fragmentShader : Shader {} Uniforms { vpos : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        
        varying vec2 vpos;
        uniform float innerWidth;
        uniform float innerHeight;
        uniform float padding;
        uniform mat4 componentMatrix;

        float linearToSRGB(float v) {
            return
                v <= 0.00313066844250063
                    ? v * 12.92
                    : 1.055 * pow(v, 1.0 / 2.4) - 0.055;
        }

        void main () {
            vec2 projected = (gl_FragCoord.xy - vec2(padding, padding)) / vec2(innerWidth, innerHeight);
            if(projected.x < 0. || projected.y < 0. || projected.x > 1. || projected.y > 1.) {
                gl_FragColor = vec4(0,0,0,1);
            }
            else {
                vec4 oklcha = componentMatrix * vec4(projected, 1, 1);
                float l = oklcha.x;
                float c = oklcha.y;
                float h = oklcha.z;
                float alpha = oklcha.w;
                float a = c * cos(h * 6.28318531);
                float b = c * sin(h * 6.28318531);
                float lOut = pow(l + 0.3963377774 * a + 0.2158037573 * b, 3.0);
                float m = pow(l - 0.1055613458 * a - 0.0638541728 * b, 3.0);
                float s = pow(l - 0.0894841775 * a - 1.291485548 * b, 3.0);
                float red = linearToSRGB(4.0767416621 * lOut - 3.3077115913 * m + 0.2309699292 * s);
                float green = linearToSRGB(-1.2684380046 * lOut + 2.6097574011 * m - 0.3413193965 * s);
                float blue = linearToSRGB(-0.0041960863 * lOut - 0.7034186147 * m + 1.707614701 * s);
                if(red < 0. || red > 1. || green < 0. || green > 1. || blue < 0. || blue > 1.) {
                    gl_FragColor = vec4(0,0,0,1);
                } else {
                    gl_FragColor = vec4(red, green, blue, alpha);
                }
            }
        }
    |]


project : Float -> Float -> Float -> Float -> Float -> Float
project fromMin fromMax toMin toMax v =
    (v - fromMin) / (fromMax - fromMin) * (toMax - toMin) + toMin


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
