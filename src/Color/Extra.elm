module Color.Extra exposing (distanceCiede2000, distanceITP, distanceOklchPlus, getHSL, getHSV, hslToHsv, setHSL, setHSV)

import Basics.Extra exposing (inDegrees)
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


distanceCiede2000 : Oklch -> Oklch -> Float
distanceCiede2000 c1 c2 =
    let
        labStar1 : { lStar : Float, aStar : Float, bStar : Float }
        labStar1 =
            toLABStar c1

        labStar2 : { lStar : Float, aStar : Float, bStar : Float }
        labStar2 =
            toLABStar c2

        lStar1 : Float
        lStar1 =
            labStar1.lStar

        aStar1 : Float
        aStar1 =
            labStar1.aStar

        bStar1 : Float
        bStar1 =
            labStar1.bStar

        lStar2 : Float
        lStar2 =
            labStar2.lStar

        aStar2 : Float
        aStar2 =
            labStar2.aStar

        bStar2 : Float
        bStar2 =
            labStar2.bStar

        lDelta : Float
        lDelta =
            deltaLPrime / (kL * sL)

        deltaLPrime : Float
        deltaLPrime =
            lStar2 - lStar1

        kL : Float
        kL =
            1

        sL : Float
        sL =
            let
                lBar : Float
                lBar =
                    (lStar1 + lStar2) / 2

                lDiff : Float
                lDiff =
                    (lBar - 50) ^ 2
            in
            1 + (0.015 * lDiff) / sqrt (20 + lDiff)

        cDelta : Float
        cDelta =
            deltaCPrime / (kC * sC)

        kC =
            1

        deltaCPrime : Float
        deltaCPrime =
            cPrime2 - cPrime1

        cPrime1 : Float
        cPrime1 =
            sqrt (aPrime1 ^ 2 + bPrime1 ^ 2)

        cPrime2 : Float
        cPrime2 =
            sqrt (aPrime2 ^ 2 + bPrime2 ^ 2)

        cStar1 : Float
        cStar1 =
            sqrt (aStar1 ^ 2 + bStar1 ^ 2)

        cStar2 : Float
        cStar2 =
            sqrt (aStar2 ^ 2 + bStar2 ^ 2)

        aPrime1 : Float
        aPrime1 =
            aStar1 * (abCorrection / 2)

        aPrime2 : Float
        aPrime2 =
            aStar2 * (abCorrection / 2)

        bPrime1 : Float
        bPrime1 =
            bStar1 * (abCorrection / 2)

        bPrime2 : Float
        bPrime2 =
            bStar2 * (abCorrection / 2)

        abCorrection : Float
        abCorrection =
            1.5 - cBarSqrt / 2

        cBarSqrt : Float
        cBarSqrt =
            sqrt (cBar ^ 7 / (cBar ^ 7 + 25 ^ 7))

        cBar : Float
        cBar =
            (cStar1 + cStar2) / 2

        sC : Float
        sC =
            1 + 0.045 * cBarPrime

        cBarPrime : Float
        cBarPrime =
            (cPrime1 + cPrime2) / 2

        hDelta : Float
        hDelta =
            deltaHPrime / (kH * sH)

        kH : Float
        kH =
            1

        deltaHPrime : Float
        deltaHPrime =
            2 * sqrt (cPrime1 * cPrime2) * sin (deltahPrime / 2)

        deltahPrime : Float
        deltahPrime =
            let
                raw : Float
                raw =
                    hPrime2 - hPrime1
            in
            if abs raw <= 180 then
                raw

            else if raw > 180 then
                raw - 360

            else
                raw + 360

        hPrime1 : Float
        hPrime1 =
            toHPrime aPrime1 bStar1

        hPrime2 : Float
        hPrime2 =
            toHPrime aPrime2 bStar2

        toHPrime : Float -> Float -> Float
        toHPrime aPrime bStar =
            if bStar == 0 && aPrime == 0 then
                0

            else
                let
                    raw : Float
                    raw =
                        inDegrees (atan2 bStar aPrime)
                in
                if raw >= 360 then
                    raw

                else
                    raw + 360

        correction : Float
        correction =
            rT * cDelta * hDelta

        rT : Float
        rT =
            -2 * cBarSqrt * sin (degrees 60 * e ^ (-(hBarPrime - 275) / 25))

        sH =
            1 + 0.015 * cBarPrime * t

        t =
            1 - 0.17 * cos (degrees (hBarPrime - 30)) + 0.24 * cos (degrees (2 * hBarPrime)) + 0.32 * cos (degrees (3 * hBarPrime + 6)) - 0.2 * cos (degrees (4 * hBarPrime - 63))

        hBarPrime =
            let
                rawAbs =
                    abs (hPrime1 - hPrime2)
            in
            if rawAbs <= 180 then
                (hPrime1 + hPrime2) / 2

            else if rawAbs > 180 && hPrime1 + hPrime2 < 360 then
                (hPrime1 + hPrime2 + 360) / 2

            else
                (hPrime1 + hPrime2 - 360) / 2
    in
    sqrt (lDelta ^ 2 + cDelta ^ 2 + hDelta ^ 2 + correction)


toLABStar : Oklch -> { lStar : Float, aStar : Float, bStar : Float }
toLABStar oklch =
    oklch
        |> Oklch.toOklab
        |> Oklab.toLinearRGB
        |> Debug.log "linearRGB"
        |> linearRGBToXYZ
        |> xyzToLABStar


xyzToLABStar : { x : Float, y : Float, z : Float } -> { lStar : Float, aStar : Float, bStar : Float }
xyzToLABStar xyz =
    let
        var_X =
            xyz.x / d65.xN

        var_Y =
            xyz.y / d65.yN

        var_Z =
            xyz.z / d65.zN

        transfer v =
            if v > 0.008856 then
                v ^ (1 / 3)

            else
                (7.787 * v) + (16 / 116)

        var_X2 =
            transfer var_X

        var_Y2 =
            transfer var_Y

        var_Z2 =
            transfer var_Z
    in
    { lStar = (116 * var_Y2) - 16
    , aStar = 500 * (var_X2 - var_Y2)
    , bStar = 200 * (var_Y2 - var_Z2)
    }


linearRGBToXYZ : LinearRGB -> { x : Float, y : Float, z : Float }
linearRGBToXYZ linearRGB =
    { x = linearRGB.linearRed * 0.4124 + linearRGB.linearGreen * 0.3576 + linearRGB.linearBlue * 0.1805
    , y = linearRGB.linearRed * 0.2126 + linearRGB.linearGreen * 0.7152 + linearRGB.linearBlue * 0.0722
    , z = linearRGB.linearRed * 0.0193 + linearRGB.linearGreen * 0.1192 + linearRGB.linearBlue * 0.9505
    }


d65 : { xN : Float, yN : Float, zN : Float }
d65 =
    { xN = 95.047
    , yN = 100.0
    , zN = 108.883
    }
