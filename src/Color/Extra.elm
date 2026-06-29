module Color.Extra exposing (distanceITP, distanceOklchPlus, getHSL, getHSV, hslToHsv, setHSL, setHSV)

import Color
import Color.LinearRGB exposing (LinearRGB)
import Color.Oklab as Oklab
import Color.Oklch as Oklch exposing (Oklch)


distanceITP : Oklch -> Oklch -> Float
distanceITP ca cb =
    let
        itpA : { i : Float, t : Float, p : Float }
        itpA =
            toITP ca

        itpB : { i : Float, t : Float, p : Float }
        itpB =
            toITP cb
    in
    sqrt
        (((itpA.i - itpB.i) ^ 2)
            + ((itpA.t - itpB.t) ^ 2)
            + ((itpA.p - itpB.p) ^ 2)
        )


distanceOklchPlus : Oklch -> Oklch -> Float
distanceOklchPlus ca cb =
    let
        oklabPlusA : { aPrime : Float, bPrime : Float, lPrime : Float }
        oklabPlusA =
            oklabPlus ca

        oklabPlusB : { aPrime : Float, bPrime : Float, lPrime : Float }
        oklabPlusB =
            oklabPlus cb

        deltaLPrime : Float
        deltaLPrime =
            oklabPlusA.lPrime - oklabPlusB.lPrime

        deltaAPrime : Float
        deltaAPrime =
            oklabPlusA.aPrime - oklabPlusB.aPrime

        deltaBPrime : Float
        deltaBPrime =
            oklabPlusA.bPrime - oklabPlusB.bPrime
    in
    sqrt (deltaLPrime ^ 2 + deltaAPrime ^ 2 + deltaBPrime ^ 2)


oklabPlus : Oklch -> { aPrime : Float, bPrime : Float, lPrime : Float }
oklabPlus oklch =
    let
        lPrime : Float
        lPrime =
            oklch.lightness ^ 0.73

        cPow : Float
        cPow =
            oklch.chroma ^ 0.87

        cPrime : Float
        cPrime =
            cPow / (cPow + 0.34 ^ 0.87)

        hPrime : Float
        hPrime =
            oklch.hue
    in
    { aPrime = cPrime * cos hPrime
    , bPrime = cPrime * sin hPrime
    , lPrime = lPrime
    }


toITP : Oklch -> { i : Float, t : Float, p : Float }
toITP oklch =
    let
        linearRGB : LinearRGB
        linearRGB =
            oklch
                |> Oklch.toOklab
                |> Oklab.toLinearRGB

        l : Float
        l =
            (1688 * linearRGB.linearRed + 2146 * linearRGB.linearGreen + 262 * linearRGB.linearBlue) / 4096

        m : Float
        m =
            (683 * linearRGB.linearRed + 2951 * linearRGB.linearGreen + 462 * linearRGB.linearBlue) / 4096

        s : Float
        s =
            (99 * linearRGB.linearRed + 309 * linearRGB.linearGreen + 3688 * linearRGB.linearBlue) / 4096

        lPrime : Float
        lPrime =
            eotfInversePQ l

        mPrime : Float
        mPrime =
            eotfInversePQ m

        sPrime : Float
        sPrime =
            eotfInversePQ s

        eotfInversePQ : Float -> Float
        eotfInversePQ fd =
            let
                m1 : Float
                m1 =
                    1305 / 8192

                m2 : Float
                m2 =
                    2523 / 32

                c1 : Float
                c1 =
                    107 / 128

                c2 : Float
                c2 =
                    2413 / 128

                c3 : Float
                c3 =
                    2392 / 128

                y : Float
                y =
                    fd / 10000

                yTom1 : Float
                yTom1 =
                    y ^ m1
            in
            ((c1 + c2 * yTom1)
                / (1 + c3 * yTom1)
            )
                ^ m2

        i : Float
        i =
            (2048 * lPrime + 2048 * mPrime) / 4096

        cT : Float
        cT =
            (6610 * lPrime - 13613 * mPrime + 7003 * sPrime) / 4096

        cP : Float
        cP =
            (17933 * lPrime - 17390 * mPrime - 543 * sPrime) / 4096
    in
    { i = i
    , t = 0.5 * cT
    , p = cP
    }


hslToHsv :
    { hue : Float, saturation : Float, lightness : Float, alpha : Float }
    -> { hue : Float, saturation : Float, value : Float, alpha : Float }
hslToHsv hsl =
    let
        v : Float
        v =
            hsl.lightness + hsl.saturation * min hsl.lightness (1 - hsl.lightness)
    in
    { hue = hsl.hue
    , saturation =
        if v == 0 then
            0

        else
            2 - 2 * hsl.lightness / v
    , value = v
    , alpha = hsl.alpha
    }


hsvToHsl :
    { hue : Float, saturation : Float, value : Float, alpha : Float }
    -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
hsvToHsl hsv =
    let
        l : Float
        l =
            hsv.value * (1 - hsv.saturation / 2)
    in
    { hue = hsv.hue
    , saturation =
        if l == 0 || l == 1 then
            0

        else
            (hsv.value - l) / min l (1 - l)
    , lightness = l
    , alpha = hsv.alpha
    }


getHSV : ({ hue : Float, saturation : Float, value : Float, alpha : Float } -> Float) -> Oklch -> Float
getHSV f c =
    Oklch.toColor c
        |> Color.toHsla
        |> hslToHsv
        |> f


setHSV :
    (Float
     -> { hue : Float, saturation : Float, value : Float, alpha : Float }
     -> { hue : Float, saturation : Float, value : Float, alpha : Float }
    )
    -> Float
    -> Oklch
    -> Oklch
setHSV f new color =
    Oklch.toColor color
        |> Color.toHsla
        |> hslToHsv
        |> f new
        |> hsvToHsl
        |> Color.fromHsla
        |> Oklch.fromColor


getHSL : ({ hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Float) -> Oklch -> Float
getHSL f c =
    c
        |> Oklch.toColor
        |> Color.toHsla
        |> f


setHSL :
    (Float
     -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
     -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
    )
    -> Float
    -> Oklch
    -> Oklch
setHSL f new color =
    Oklch.toColor color |> Color.toHsla |> f new |> Color.fromHsla |> Oklch.fromColor
