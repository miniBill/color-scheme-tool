module Main exposing (ColorSpace, Model, Msg, Palette, main)

import Brewer
import Browser
import Color
import Color.Extra exposing (getHSL, getHSV, setHSL, setHSV)
import Color.LinearRGB exposing (LinearRGB)
import Color.Oklab as Oklab exposing (Oklab)
import Color.Oklch as Oklch exposing (Oklch)
import FathersDay
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra
import List.NonEmpty
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
    , hoveredColor : Maybe Oklch
    , colorSpace : ColorSpace
    }


type Msg
    = Palette Palette
    | ColorSpace ColorSpace
    | HoverColor (Maybe Oklch)
    | SelectColor Oklch


type ColorSpace
    = OKLCH
    | OKLAB
    | SRGB
    | HSL
    | HSV


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
        greedyPalette 31 ( Oklab.oklab 1 0 0, [] )
            |> List.NonEmpty.toList
            |> List.map Oklch.fromOklab

    -- |> List.sortBy .hue
    , selectedColor = Nothing
    , hoveredColor = Nothing
    , colorSpace = OKLCH
    }


greedyPalette : Int -> ( Oklab, List Oklab ) -> ( Oklab, List Oklab )
greedyPalette n (( h, t ) as acc) =
    if n <= 0 then
        acc

    else
        case greedyPaletteHelp 0 0 0 Nothing 0 acc of
            Just found ->
                greedyPalette (n - 1) ( found, h :: t )

            Nothing ->
                acc


greedyPaletteHelp : Int -> Int -> Int -> Maybe Oklab -> Float -> ( Oklab, List Oklab ) -> Maybe Oklab
greedyPaletteHelp r g b best bestDistanceSquared (( h, t ) as acc) =
    let
        step : number
        step =
            8

        candidate : Oklab
        candidate =
            Color.rgb255 r g b
                |> Oklab.fromColor

        distanceSquared : Oklab -> Float
        distanceSquared p =
            oklabDistanceSquared candidate p

        minDistanceSquared : Float
        minDistanceSquared =
            List.foldl (\e a -> min a (distanceSquared e)) (distanceSquared h) t

        ( nextBest, nextBestDistanceSquared ) =
            if minDistanceSquared > bestDistanceSquared then
                ( Just candidate, minDistanceSquared )

            else
                ( best, bestDistanceSquared )
    in
    if (b + step) < 256 then
        greedyPaletteHelp r g (b + step) nextBest nextBestDistanceSquared acc

    else if (g + step) < 256 then
        greedyPaletteHelp r (g + step) 0 nextBest nextBestDistanceSquared acc

    else if (r + step) < 256 then
        greedyPaletteHelp (r + step) 0 0 nextBest nextBestDistanceSquared acc

    else
        best


oklabDistanceSquared : Oklab -> Oklab -> Float
oklabDistanceSquared candidate p =
    ((candidate.lightness - p.lightness) ^ 2)
        + ((candidate.a - p.a) ^ 2)
        + ((candidate.b - p.b) ^ 2)


uniformPalette : List Oklch
uniformPalette =
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


view : Model -> Html Msg
view model =
    Theme.column
        [ Theme.padding ]
        [ [ OKLCH, OKLAB, SRGB, HSL, HSV ]
            |> List.map
                (\colorSpace ->
                    Html.button
                        [ Html.Events.onClick (ColorSpace colorSpace)
                        , if colorSpace == model.colorSpace then
                            Html.Attributes.style "box-shadow" "0 0 4px 4px rgba(0, 149, 255, 0.5)"

                          else
                            Html.Attributes.style "" ""
                        ]
                        [ Html.text (colorSpaceToString colorSpace) ]
                )
            |> Theme.wrappedRow []
        , (case model.colorSpace of
            OKLCH ->
                [ ( oklchComponents.l, oklchComponents.c, oklchComponents.h )
                , ( oklchComponents.h, oklchComponents.c, oklchComponents.l )
                , ( oklchComponents.h, oklchComponents.l, oklchComponents.c )
                ]

            OKLAB ->
                [ ( oklabComponents.l, oklabComponents.a, oklabComponents.b )
                , ( oklabComponents.l, oklabComponents.b, oklabComponents.a )
                , ( oklabComponents.a, oklabComponents.b, oklabComponents.l )
                ]

            SRGB ->
                [ ( rgbComponents.r, rgbComponents.g, rgbComponents.b )
                , ( rgbComponents.g, rgbComponents.b, rgbComponents.r )
                , ( rgbComponents.b, rgbComponents.r, rgbComponents.g )
                ]

            HSL ->
                [ ( hslComponents.l, hslComponents.s, hslComponents.h )
                , ( hslComponents.h, hslComponents.s, hslComponents.l )
                , ( hslComponents.h, hslComponents.l, hslComponents.s )
                ]

            HSV ->
                [ ( hsvComponents.v, hsvComponents.s, hsvComponents.h )
                , ( hsvComponents.h, hsvComponents.s, hsvComponents.v )
                , ( hsvComponents.h, hsvComponents.v, hsvComponents.s )
                ]
          )
            |> List.map (\( cx, cy, cm ) -> viewSlice model cx cy cm model.current)
            |> Theme.wrappedRow []
        , viewHorizontalPalette model
        , viewPalettes model
        ]


viewHorizontalPalette : Model -> Html Msg
viewHorizontalPalette model =
    model.current
        |> List.map (colorDiv model)
        |> Theme.wrappedRow []


colorSpaceToString : ColorSpace -> String
colorSpaceToString colorSpace =
    case colorSpace of
        OKLCH ->
            "OKLCH"

        OKLAB ->
            "OKLAB"

        SRGB ->
            "sRGB"

        HSL ->
            "HSL"

        HSV ->
            "HSV"


type alias Component =
    { get : Oklch -> Float
    , set : Float -> Oklch -> Oklch
    , min : Float
    , max : Float
    , default : Float
    , component : Vec3
    , label : String
    }


oklchComponents : { l : Component, c : Component, h : Component }
oklchComponents =
    { l =
        { get = .lightness
        , set = \new color -> { color | lightness = new }
        , min = 0
        , max = 1
        , default = 0.7
        , component = Vector3.vec3 1 0 0
        , label = "L"
        }
    , c =
        { get = .chroma
        , set = \new color -> { color | chroma = new }
        , min = 0
        , max = 0.37
        , default = 0.1
        , component = Vector3.vec3 0 1 0
        , label = "C"
        }
    , h =
        { get = .hue
        , set = \new color -> { color | hue = new }
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 0 0 1
        , label = "H"
        }
    }


oklabComponents : { l : Component, a : Component, b : Component }
oklabComponents =
    let
        getOklab : (Oklab -> Float) -> Oklch -> Float
        getOklab f c =
            f (Oklch.toOklab c)

        setOklab : (Float -> Oklab -> Oklab) -> Float -> Oklch -> Oklch
        setOklab f new color =
            Oklch.fromOklab (f new (Oklch.toOklab color))
    in
    { l =
        { get = getOklab .lightness
        , set = setOklab (\new color -> { color | lightness = new })
        , min = 0
        , max = 1
        , default = 0.7
        , component = Vector3.vec3 1 0 0
        , label = "L"
        }
    , a =
        { get = getOklab .a
        , set = setOklab (\new color -> { color | a = new })
        , min = -0.3
        , max = 0.3
        , default = 0
        , component = Vector3.vec3 0 1 0
        , label = "A"
        }
    , b =
        { get = getOklab .b
        , set = setOklab (\new color -> { color | b = new })
        , min = -0.3
        , max = 0.3
        , default = 0
        , component = Vector3.vec3 0 0 1
        , label = "B"
        }
    }


rgbComponents : { r : Component, g : Component, b : Component }
rgbComponents =
    let
        getsRGB : ({ red : Float, green : Float, blue : Float, alpha : Float } -> Float) -> Oklch -> Float
        getsRGB f c =
            c
                |> Oklch.toColor
                |> Color.toRgba
                |> f

        setsRGB :
            (Float
             -> { red : Float, green : Float, blue : Float, alpha : Float }
             -> { red : Float, green : Float, blue : Float, alpha : Float }
            )
            -> Float
            -> Oklch
            -> Oklch
        setsRGB f new color =
            Oklch.toColor color
                |> Color.toRgba
                |> f new
                |> Color.fromRgba
                |> Oklch.fromColor
    in
    { r =
        { get = getsRGB .red
        , set = setsRGB (\new color -> { color | red = new })
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 1 0 0
        , label = "R"
        }
    , g =
        { get = getsRGB .green
        , set = setsRGB (\new color -> { color | green = new })
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 0 1 0
        , label = "G"
        }
    , b =
        { get = getsRGB .blue
        , set = setsRGB (\new color -> { color | blue = new })
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 0 0 1
        , label = "B"
        }
    }


hslComponents : { h : Component, s : Component, l : Component }
hslComponents =
    { h =
        { get = getHSL .hue
        , set = setHSL (\new color -> { color | hue = new })
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 1 0 0
        , label = "H"
        }
    , s =
        { get = getHSL .saturation
        , set = setHSL (\new color -> { color | saturation = new })
        , min = 0
        , max = 1
        , default = 0.7
        , component = Vector3.vec3 0 1 0
        , label = "S"
        }
    , l =
        { get = getHSL .lightness
        , set = setHSL (\new color -> { color | lightness = new })
        , min = 0
        , max = 1
        , default = 0.5
        , component = Vector3.vec3 0 0 1
        , label = "L"
        }
    }


hsvComponents : { h : Component, s : Component, v : Component }
hsvComponents =
    { h =
        { get = getHSV .hue
        , set = setHSV (\new color -> { color | hue = new })
        , min = 0
        , max = 1
        , default = 0
        , component = Vector3.vec3 1 0 0
        , label = "H"
        }
    , s =
        { get = getHSV .saturation
        , set = setHSV (\new color -> { color | saturation = new })
        , min = 0
        , max = 1
        , default = 0.7
        , component = Vector3.vec3 0 1 0
        , label = "S"
        }
    , v =
        { get = getHSV .value
        , set = setHSV (\new color -> { color | value = new })
        , min = 0
        , max = 1
        , default = 1
        , component = Vector3.vec3 0 0 1
        , label = "V"
        }
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
                                    |> project xComponent.min xComponent.max 0 innerWidth
                                    |> String.fromFloat
                                )
                            , Svg.Attributes.cy
                                (yComponent.get color
                                    |> project yComponent.min yComponent.max innerHeight 0
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
                            , Svg.Events.onMouseOver (HoverColor (Just color))
                            , Svg.Events.onMouseOut (HoverColor Nothing)
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
                    { componentMatrix = componentMatrix model xComponent yComponent missingComponent palette
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


componentMatrix : Model -> Component -> Component -> Component -> Palette -> Mat4
componentMatrix model xComponent yComponent missingComponent palette =
    let
        xSpan : Float
        xSpan =
            xComponent.max - xComponent.min

        ySpan : Float
        ySpan =
            yComponent.max - yComponent.min

        missingComponentValue : Float
        missingComponentValue =
            case model.hoveredColor of
                Just color ->
                    missingComponent.get color

                Nothing ->
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

        c1 : { x : Float, y : Float, z : Float }
        c1 =
            Vector3.toRecord xComponent.component

        c2 : { x : Float, y : Float, z : Float }
        c2 =
            Vector3.toRecord yComponent.component

        c3 : { x : Float, y : Float, z : Float }
        c3 =
            Vector3.toRecord missingComponent.component

        colorSpaceFloat : number
        colorSpaceFloat =
            case model.colorSpace of
                OKLCH ->
                    1

                OKLAB ->
                    2

                SRGB ->
                    3

                HSL ->
                    4

                HSV ->
                    5
    in
    Matrix4.fromRecord
        { m11 = xSpan * c1.x
        , m12 = ySpan * c2.x
        , m13 = missingComponentValue * c3.x
        , m14 = c1.x * xComponent.min + c2.x * yComponent.min
        , m21 = xSpan * c1.y
        , m22 = ySpan * c2.y
        , m23 = missingComponentValue * c3.y
        , m24 = c1.y * xComponent.min + c2.y * yComponent.min
        , m31 = xSpan * c1.z
        , m32 = ySpan * c2.z
        , m33 = missingComponentValue * c3.z
        , m34 = c1.z * xComponent.min + c2.z * yComponent.min
        , m41 = 0
        , m42 = 0
        , m43 = 0
        , m44 = colorSpaceFloat
        }


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

        vec3 oklchToOkLab(vec3 oklch) {
            return vec3(
                oklch.x,
                oklch.y * cos(oklch.z * 6.28318531),
                oklch.y * sin(oklch.z * 6.28318531)
            );
        }

        float hueToRgb(float m1, float m2, float h) {
            h = mod(h, 1.);

            if (h * 6. < 1.) {
                return m1 + (m2 - m1) * h * 6.;
            } else if (h * 2. < 1.) {
                return m2;
            } else if (h * 3. < 2.) {
                return m1 + (m2 - m1) * (2. / 3. - h) * 6.;
            } else {
                return m1;
            } 
        }

        vec3 hslToSRGB(vec3 hsl) {
            float h = hsl.x;
            float s = hsl.y;
            float l = hsl.z;

            float m2 =
                l <= 0.5
                    ? l * (s + 1.)
                    : l + s - l * s;
            float m1 = l * 2. - m2;

            float r = hueToRgb(m1, m2, h + 1. / 3.);
            float g = hueToRgb(m1, m2, h);
            float b = hueToRgb(m1, m2, h - 1. / 3.);

            return vec3(r, g, b);
        }

        vec3 hsvToHsl(vec3 hsv) {
            hsv = clamp(hsv, 0., 1.);
            float h = hsv.x;
            float s = hsv.y;
            float v = hsv.z;

            float l = v * (1. - s / 2.);
            float s_ = (l == 0. || l == 1.) ? 0. : ((v - l) / min(l, 1. - l));

            return vec3(h, s_, l);
        }

        void main () {
            vec2 projected = (gl_FragCoord.xy - vec2(padding, padding)) / vec2(innerWidth, innerHeight);
            vec3 sRGB = vec3(0);
            if (clamp(projected, 0., 1.) == projected) {
                vec4 components = componentMatrix * vec4(projected, 1, 1);
                if (components.w == 1.) {
                    vec3 oklch = components.xyz;
                    vec3 oklab = oklchToOkLab(oklch);
                    vec3 linearRGB = labToLinearRGB(oklab);
                    sRGB = linearRGBToSRGB(linearRGB);
                } else if (components.w == 2.) {
                    vec3 oklab = components.xyz;
                    vec3 linearRGB = labToLinearRGB(oklab);
                    sRGB = linearRGBToSRGB(linearRGB);
                } else if (components.w == 3.) {
                    sRGB = components.xyz;
                } else if (components.w == 4.) {
                    vec3 hsl = components.xyz;
                    sRGB = hslToSRGB(hsl);
                } else if (components.w == 5.) {
                    vec3 hsv = components.xyz;
                    vec3 hsl = hsvToHsl(hsv);
                    sRGB = hslToSRGB(hsl);
                }
            }
            vec3 clamped = clamp(sRGB, -0.0001, 1.0001);
            gl_FragColor = (clamped == sRGB) ? vec4(sRGB, 1) : vec4(0,0,0,1);
            //if(clamped != sRGB) {
            //    if(sRGB.x < -0.0001) {
            //        gl_FragColor = vec4(1,0,0,1);
            //    } else if (sRGB.x > 1.0001) {
            //        gl_FragColor = vec4(0,1,1,1);
            //    } else if (sRGB.y < -0.0001) {
            //        gl_FragColor = vec4(0,1,0,1);
            //    } else if (sRGB.y > 1.0001) {
            //        gl_FragColor = vec4(1,0,1,1);
            //    } else if (sRGB.z < -0.0001) {
            //        gl_FragColor = vec4(0,0,1,1);
            //    } else if (sRGB.z > 1.0001) {
            //        gl_FragColor = vec4(1,1,0,1);
            //    } else {
            //        gl_FragColor = vec4(1);
            //    }
            //}
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
                viewPalette model { selected = model.current == palette } model.colorSpace palette
            )
        |> Theme.wrappedRow [ Html.Attributes.style "align-items" "start" ]


viewPalette : Model -> { selected : Bool } -> ColorSpace -> Palette -> Html Msg
viewPalette model { selected } colorSpace palette =
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
                [ Html.Attributes.style "box-shadow" "0px 0px 4px 4px rgba(0, 149, 255, 0.5)"
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
            , ( [ "oklch", "oklab", "rgb", "name" ], "auto" )
            , ( [ "l", "r", "first" ], "auto" )
            , ( [], "8px" )
            , ( [ "c", "a", "g", "second" ], "auto" )
            , ( [], "8px" )
            , ( [ "h", "b", "third" ], "auto" )
            , ( [], "16px" )
            ]

        children : List (Html Msg)
        children =
            palette
                |> List.indexedMap
                    (\i color ->
                        viewColor model { selected = selected } colorSpace color
                     -- |> List.map
                     --     (Html.map
                     --         (\newColor ->
                     --             Palette (List.Extra.setAt i newColor palette)
                     --         )
                     --     )
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

                    minDeltas : List (Html Msg)
                    minDeltas =
                        let
                            take : List a -> List a
                            take =
                                List.take 5

                            deltas : List ( Float, Oklch, Oklch )
                            deltas =
                                palette
                                    |> List.Extra.uniquePairs
                                    |> List.map
                                        (\( ca, cb ) ->
                                            ( Color.Extra.distanceOklchPlus ca cb
                                              -- ( Color.Extra.distanceITP ca cb
                                            , ca
                                            , cb
                                            )
                                        )
                                    |> List.sortBy Triple.Extra.first

                            darkest : List ( Float, Oklch, Oklch )
                            darkest =
                                deltas
                                    |> List.Extra.removeWhen (\p -> List.member p (take deltas))
                                    |> List.sortBy (\( _, ca, cb ) -> ca.lightness + cb.lightness)
                        in
                        (take deltas ++ take darkest)
                            |> List.sortBy Triple.Extra.first
                            |> List.map
                                (\( delta, ca, cb ) ->
                                    Html.div []
                                        [ Html.text "ΔE"

                                        -- , Html.sub [] [ Html.text "ITP" ]
                                        , Html.sub [] [ Html.text "Oklch+" ]
                                        , Html.text "( "
                                        , colorDiv model ca
                                        , Html.text " , "
                                        , colorDiv model cb
                                        , Html.text (" ) = " ++ Round.round 3 delta)
                                        ]
                                )
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
                    , Theme.gap
                    , Html.Attributes.style "grid-template-columns" "auto auto auto auto"
                    ]
                    (List.map
                        (\label ->
                            Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text label ]
                        )
                        [ "Component", "Min", "Max", "Range" ]
                        ++ List.concatMap componentInfo [ oklchComponents.l, oklchComponents.c, oklchComponents.h ]
                    )
                , Html.div
                    [ spanColumns
                    , Html.Attributes.style "text-align" "center"
                    ]
                    minDeltas
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


viewColor : Model -> { selected : Bool } -> ColorSpace -> Oklch -> List (Html Msg)
viewColor model { selected } colorSpace color =
    if selected then
        case colorSpace of
            OKLCH ->
                [ colorDiv model color
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
                [ colorDiv model color
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
                [ colorDiv model color
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

            HSL ->
                let
                    hsl : { hue : Float, saturation : Float, lightness : Float, alpha : Float }
                    hsl =
                        color
                            |> Oklch.toColor
                            |> Color.toHsla
                in
                [ colorDiv model color
                , Html.span
                    [ Html.Attributes.style "grid-column" "rgb" ]
                    [ Html.text "hsl(" ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "r"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsl.hue * 360) ++ "deg ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "g"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsl.saturation * 100) ++ "% ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "b"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsl.lightness * 100) ++ "%)") ]
                ]

            HSV ->
                let
                    hsv : { hue : Float, saturation : Float, value : Float, alpha : Float }
                    hsv =
                        color
                            |> Oklch.toColor
                            |> Color.toHsla
                            |> Color.Extra.hslToHsv
                in
                [ colorDiv model color
                , Html.span
                    [ Html.Attributes.style "grid-column" "rgb" ]
                    [ Html.text "hsv(" ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "r"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsv.hue * 360) ++ "deg ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "g"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsv.saturation * 100) ++ "% ") ]
                , Html.span
                    [ Html.Attributes.style "grid-column" "b"
                    , Html.Attributes.style "justify-self" "right"
                    ]
                    [ Html.text (Round.round 0 (hsv.value * 100) ++ "%)") ]
                ]

    else
        [ colorDiv model color ]


colorDiv : Model -> Oklch -> Html Msg
colorDiv model color =
    Html.div
        [ Html.Attributes.style "background-color" (Oklch.toCssString color)
        , Html.Attributes.style "grid-column" "color"
        , Html.Attributes.style "width" "24px"
        , Html.Attributes.style "height" "24px"
        , Html.Attributes.style "display" "inline-block"
        , Html.Attributes.title
            ("oklch("
                ++ Round.round 0 (color.lightness * 100)
                ++ "% "
                ++ Round.round 3 color.chroma
                ++ " "
                ++ Round.round 3 (360 * color.hue)
                ++ ")"
            )
        , Html.Events.onMouseEnter (HoverColor (Just color))
        , Html.Events.onMouseLeave (HoverColor Nothing)
        , Html.Events.onClick (SelectColor color)
        , if model.selectedColor == Just color then
            Html.Attributes.style "box-shadow" "0 0 2px 2px rgba(0, 141, 180, 0.63)"

          else
            Html.Attributes.style "box-shadow" "initial"
        , Html.Attributes.style "cursor" "pointer"
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Palette palette ->
            { model | current = palette }

        ColorSpace colorSpace ->
            { model | colorSpace = colorSpace }

        HoverColor color ->
            { model | hoveredColor = color }

        SelectColor color ->
            if Just color == model.selectedColor then
                { model | selectedColor = Nothing }

            else
                { model | selectedColor = Just color }
