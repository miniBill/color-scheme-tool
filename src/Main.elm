module Main exposing (ColorSpace, Model, Msg, Palette, main)

import Brewer
import Browser
import Color
import Color.Oklab exposing (Oklab)
import Color.Oklch as Oklch exposing (Oklch)
import FathersDay
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import Round
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Theme
import Triple.Extra
import WebGL exposing (Mesh, Shader)


type alias Palette =
    List Oklch


type alias Model =
    { current : Palette
    , selectedColor : Maybe Oklch
    , colorSpace : ColorSpace
    }


type Msg
    = Palette Palette
    | ColorSpace ColorSpace
    | SelectColor Oklch


type ColorSpace
    = OKLCH
    | OKLAB
    | SRGB


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { current =
        List.range 0 31
            |> List.map
                (\index ->
                    let
                        hueCount : number
                        hueCount =
                            8

                        ( x, y, z ) =
                            ( modBy hueCount index
                            , (index // hueCount) |> modBy 2
                            , index // (hueCount * 2)
                            )

                        l : Float
                        l =
                            project 0 1 0.5 0.8 (toFloat y)

                        c : Float
                        c =
                            project 0 1 0.075 0.15 (toFloat z)

                        h : Float
                        h =
                            project 0 (hueCount - 1) 0 ((hueCount - 1) / hueCount) (toFloat x)
                                + (1 / (hueCount * 2))
                    in
                    Oklch.oklch l c h
                )
            |> List.sortBy .hue
            |> List.filter (\{ lightness } -> lightness == 0.8)
    , selectedColor = Nothing
    , colorSpace = OKLCH
    }


view : Model -> Html Msg
view model =
    Theme.column
        [ Theme.padding ]
        [ [ OKLCH, OKLAB, SRGB ]
            |> List.map
                (\colorSpace ->
                    Html.button
                        [ Html.Events.onClick (ColorSpace colorSpace) ]
                        [ Html.text (colorSpaceToString colorSpace) ]
                )
            |> Theme.wrappedRow []
        , Theme.wrappedRow []
            [ viewSlice model lComponent cComponent hComponent model.current
            , viewSlice model hComponent cComponent lComponent model.current
            , viewSlice model hComponent lComponent cComponent model.current
            ]
        , viewHorizontalPalette model.current
        , viewPalettes model
        ]


viewHorizontalPalette : Palette -> Html Msg
viewHorizontalPalette palette =
    palette |> List.map colorDiv |> Theme.wrappedRow []


colorSpaceToString : ColorSpace -> String
colorSpaceToString colorSpace =
    case colorSpace of
        OKLCH ->
            "OKLCH"

        OKLAB ->
            "OKLAB"

        SRGB ->
            "sRGB"


type alias Component =
    { get : Oklch -> Float
    , set : Float -> Oklch -> Oklch
    , max : Float
    , default : Float
    , component : Vec3
    , label : String
    }


lComponent : Component
lComponent =
    { get = .lightness
    , set = \new color -> { color | lightness = new }
    , max = 1
    , default = 0.7
    , component = Vector3.vec3 1 0 0
    , label = "L"
    }


cComponent : Component
cComponent =
    { get = .chroma
    , set = \new color -> { color | chroma = new }
    , max = 0.37
    , default = 0.1
    , component = Vector3.vec3 0 1 0
    , label = "C"
    }


hComponent : Component
hComponent =
    { get = .hue
    , set = \new color -> { color | hue = new }
    , max = 1
    , default = 0
    , component = Vector3.vec3 0 0 1
    , label = "H"
    }


viewSlice : Model -> Component -> Component -> Component -> Palette -> Html Msg
viewSlice model xComponent yComponent missingComponent palette =
    let
        padding : number
        padding =
            25

        innerWidth : number
        innerWidth =
            400

        outerWidth : number
        outerWidth =
            innerWidth + 2 * padding

        innerHeight : number
        innerHeight =
            275

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

        dots : List (Svg Msg)
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
                            , if model.selectedColor == Just color then
                                Svg.Attributes.stroke "white"

                              else
                                Svg.Attributes.stroke "black"
                            , Svg.Events.onClick (SelectColor color)
                            , Svg.Attributes.cursor "pointer"
                            ]
                            []
                    )

        svg : Html Msg
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

        missingComponentValue : Float
        missingComponentValue =
            case model.selectedColor of
                Just color ->
                    missingComponent.get color

                Nothing ->
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
                            (Vector3.scale missingComponentValue missingComponent.component)
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

        float linearRGBToSRGB(float v) {
            return
                v <= 0.00313066844250063
                    ? v * 12.92
                    : 1.055 * pow(v, 1.0 / 2.4) - 0.055;
        }

        vec3 linearRGBToSRGB(vec3 linearRGB) {
            return vec3(
                linearRGBToSRGB(linearRGB.r),
                linearRGBToSRGB(linearRGB.g),
                linearRGBToSRGB(linearRGB.b)
            );
        }

        vec3 labToLinearRGB(vec3 lab) {
            mat3 labToLmsRoot = mat3(
                1.0,           1.0,           1.0,
                0.3963377774, -0.1055613458, -0.0894841775,
                0.2158037573, -0.0638541728, -1.2914855480
            );
            vec3 lmsRoot = labToLmsRoot * lab;
            vec3 lms = pow(lmsRoot, vec3(3.0));
            mat3 lmsToLinearRGB = mat3(
                 4.0767416621, -1.2684380046, -0.0041960863,
                -3.3077115913,  2.6097574011, -0.7034186147,
                 0.2309699292, -0.3413193965,  1.7076147010
            );
            return lmsToLinearRGB * lms;
        }

        vec3 oklchToLab(vec3 oklch) {
            return vec3(
                oklch.x,
                oklch.y * cos(oklch.z * 6.28318531),
                oklch.y * sin(oklch.z * 6.28318531)
            );
        }

        void main () {
            vec2 projected = (gl_FragCoord.xy - vec2(padding, padding)) / vec2(innerWidth, innerHeight);
            if(projected.x < 0. || projected.y < 0. || projected.x > 1. || projected.y > 1.) {
                gl_FragColor = vec4(0,0,0,1);
            }
            else {
                vec3 oklch = (componentMatrix * vec4(projected, 1, 1)).xyz;
                vec3 lab = oklchToLab(oklch);
                vec3 linearRGB = labToLinearRGB(lab);
                vec3 sRGB = linearRGBToSRGB(linearRGB);
                vec3 clamped = clamp(sRGB, 0., 1.);
                gl_FragColor = (clamped == sRGB) ? vec4(sRGB, 1) : vec4(0,0,0,1);
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
    (if List.member model.current all then
        all

     else
        model.current :: all
    )
        |> List.map
            (\palette ->
                viewPalette { selected = model.current == palette } model.colorSpace palette
            )
        |> Theme.wrappedRow [ Html.Attributes.style "align-items" "start" ]


viewPalette : { selected : Bool } -> ColorSpace -> Palette -> Html Msg
viewPalette { selected } colorSpace palette =
    let
        attrs : List (Attribute Msg)
        attrs =
            commonAttrs ++ selectionAttrs

        commonAttrs : List (Attribute Msg)
        commonAttrs =
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "gap" "8px 0"
            , Html.Events.onClick (Palette palette)
            ]

        selectionAttrs : List (Attribute Msg)
        selectionAttrs =
            if selected then
                [ Html.Attributes.style "box-shadow" "0px 0px 4px 4px #ccf"
                , Html.Attributes.style "background" "#f0f0ff"
                , gridTemplate Column columns
                ]

            else
                [ gridTemplate Column
                    [ ( [ "color" ], "24px" )
                    ]
                ]

        columns : List ( List String, String )
        columns =
            [ ( [ "color" ], "24px" )
            , ( [], "8px" )
            , ( [ "oklch", "oklab", "rgb" ], "auto" )
            , ( [ "l", "r" ], "auto" )
            , ( [], "8px" )
            , ( [ "c", "a", "g" ], "auto" )
            , ( [], "8px" )
            , ( [ "h", "b" ], "auto" )
            , ( [], "16px" )
            ]

        children : List (Html Msg)
        children =
            palette
                |> List.indexedMap
                    (\i color ->
                        viewColor { selected = selected } colorSpace color
                            |> List.map
                                (Html.map
                                    (\newColor ->
                                        Palette (List.Extra.setAt i newColor palette)
                                    )
                                )
                    )
                |> List.concat

        header : List (Html Msg)
        header =
            if selected then
                let
                    spanColumns : Attribute msg
                    spanColumns =
                        Html.Attributes.style "grid-column"
                            ("1 / span " ++ String.fromInt (List.length columns))

                    minDelta : List (Html Msg)
                    minDelta =
                        palette
                            |> List.map Oklch.toOklab
                            |> List.Extra.uniquePairs
                            |> List.map
                                (\( ca, cb ) ->
                                    ( sqrt
                                        (((ca.a - cb.a) ^ 2)
                                            + ((ca.b - cb.b) ^ 2)
                                            + ((ca.lightness - cb.lightness) ^ 2)
                                        )
                                    , ca
                                    , cb
                                    )
                                )
                            |> List.Extra.minimumBy Triple.Extra.first
                            |> Maybe.map
                                (\( delta, ca, cb ) ->
                                    [ Html.text (Round.round 3 delta ++ " between ")
                                    , colorDiv (Oklch.fromOklab ca)
                                    , Html.text " and "
                                    , colorDiv (Oklch.fromOklab cb)
                                    ]
                                )
                            |> Maybe.withDefault [ Html.text "—" ]
                in
                [ Html.button
                    [ spanColumns
                    , Json.Decode.succeed ( Palette (List.sortBy .hue palette), True )
                        |> Html.Events.stopPropagationOn "click"
                    ]
                    [ Html.text "Sort by hue" ]
                , Html.div
                    [ spanColumns
                    , Html.Attributes.style "display" "grid"
                    , Html.Attributes.style "grid-template-columns" "auto auto auto auto"
                    ]
                    (List.map
                        (\label ->
                            Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text label ]
                        )
                        [ "Component", "Min", "Max", "Range" ]
                        ++ List.concatMap componentInfo [ lComponent, cComponent, hComponent ]
                    )
                , Html.div
                    [ spanColumns ]
                    (Html.text "Min Δ in Lab: " :: minDelta)
                ]

            else
                []

        componentInfo : Component -> List (Html Msg)
        componentInfo component =
            let
                list : List Float
                list =
                    List.map component.get palette

                floatCell : Maybe Float -> Html Msg
                floatCell v =
                    v
                        |> Maybe.map (Round.round 3)
                        |> Maybe.withDefault "—"
                        |> Html.text
                        |> List.singleton
                        |> Html.span []
            in
            [ Html.span [] [ Html.text component.label ]
            , floatCell (List.minimum list)
            , floatCell (List.maximum list)
            , floatCell (Maybe.map2 (-) (List.maximum list) (List.minimum list))
            ]
    in
    Theme.box attrs
        (header ++ children)


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


viewColor : { selected : Bool } -> ColorSpace -> Oklch -> List (Html Oklch)
viewColor { selected } colorSpace color =
    if selected then
        case colorSpace of
            OKLCH ->
                [ colorDiv color
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
                ]

            OKLAB ->
                let
                    oklab : Oklab
                    oklab =
                        Oklch.toOklab color
                in
                [ colorDiv color
                , Html.span
                    [ Html.Attributes.style "grid-column" "oklab" ]
                    [ Html.text "oklab(" ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "l"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (oklab.lightness * 100) ++ "% ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "a"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 3 oklab.a ++ " ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "b"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 3 oklab.b ++ ")") ]
                ]

            SRGB ->
                let
                    rgb : { red : Float, green : Float, blue : Float, alpha : Float }
                    rgb =
                        color
                            |> Oklch.toColor
                            |> Color.toRgba
                in
                [ colorDiv color
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
                    [ Html.text (Round.round 0 (rgb.blue * 100) ++ "%)") ]
                ]

    else
        [ colorDiv color ]


colorDiv : Oklch -> Html msg
colorDiv color =
    Html.div
        [ Html.Attributes.style "background-color" (Oklch.toCssString color)
        , Html.Attributes.style "grid-column" "color"
        , Html.Attributes.style "width" "24px"
        , Html.Attributes.style "height" "24px"
        , Html.Attributes.style "display" "inline-block"
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Palette palette ->
            { model | current = palette }

        ColorSpace colorSpace ->
            { model | colorSpace = colorSpace }

        SelectColor color ->
            if Just color == model.selectedColor then
                { model | selectedColor = Nothing }

            else
                { model | selectedColor = Just color }
