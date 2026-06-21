module Brewer exposing (accent, all, dark2, paired, pastel1, pastel2, set1, set2, set3)

{-| Categorical color palettes by Cynthia Brewer
-}

import Bitwise
import Color exposing (Color)
import Hex


all =
    [ accent
    , dark2
    , paired
    , pastel1
    , pastel2
    , set1
    , set2
    , set3
    ]


accent : List Color
accent =
    toPalette
        [ "#7fc97f"
        , "#beaed4"
        , "#fdc086"
        , "#ffff99"
        , "#386cb0"
        , "#f0027f"
        , "#bf5b17"
        , "#666666"
        ]


dark2 : List Color
dark2 =
    toPalette
        [ "#1b9e77"
        , "#d95f02"
        , "#7570b3"
        , "#e7298a"
        , "#66a61e"
        , "#e6ab02"
        , "#a6761d"
        , "#666666"
        ]


paired : List Color
paired =
    toPalette
        [ "#a6cee3"
        , "#1f78b4"
        , "#b2df8a"
        , "#33a02c"
        , "#fb9a99"
        , "#e31a1c"
        , "#fdbf6f"
        , "#ff7f00"
        , "#cab2d6"
        , "#6a3d9a"
        , "#ffff99"
        , "#b15928"
        ]


pastel1 : List Color
pastel1 =
    toPalette
        [ "#fbb4ae"
        , "#b3cde3"
        , "#ccebc5"
        , "#decbe4"
        , "#fed9a6"
        , "#ffffcc"
        , "#e5d8bd"
        , "#fddaec"
        , "#f2f2f2"
        ]


pastel2 : List Color
pastel2 =
    toPalette
        [ "#b3e2cd"
        , "#fdcdac"
        , "#cbd5e8"
        , "#f4cae4"
        , "#e6f5c9"
        , "#fff2ae"
        , "#f1e2cc"
        , "#cccccc"
        ]


set1 : List Color
set1 =
    toPalette
        [ "#e41a1c"
        , "#377eb8"
        , "#4daf4a"
        , "#984ea3"
        , "#ff7f00"
        , "#ffff33"
        , "#a65628"
        , "#f781bf"
        , "#999999"
        ]


set2 : List Color
set2 =
    toPalette
        [ "#66c2a5"
        , "#fc8d62"
        , "#8da0cb"
        , "#e78ac3"
        , "#a6d854"
        , "#ffd92f"
        , "#e5c494"
        , "#b3b3b3"
        ]


set3 : List Color
set3 =
    toPalette
        [ "#8dd3c7"
        , "#ffffb3"
        , "#bebada"
        , "#fb8072"
        , "#80b1d3"
        , "#fdb462"
        , "#b3de69"
        , "#fccde5"
        , "#d9d9d9"
        , "#bc80bd"
        , "#ccebc5"
        , "#ffed6f"
        ]


toPalette : List String -> List Color
toPalette colors =
    List.map toColor colors


toColor : String -> Color
toColor input =
    let
        hex =
            Hex.fromString (String.dropLeft 1 input)
    in
    case hex of
        Ok h ->
            Color.rgb255
                (h |> Bitwise.shiftRightBy 16 |> Bitwise.and 0xFF)
                (h |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF)
                (h |> Bitwise.shiftRightBy 0 |> Bitwise.and 0xFF)

        Err _ ->
            Color.rgb 1 0 1
