module Theme exposing (box, column, gap, row, wrappedRow)

import Html exposing (Attribute, Html)
import Html.Attributes


gap : Attribute msg
gap =
    Html.Attributes.style "gap" "8px"


box : List (Attribute msg) -> List (Html msg) -> Html msg
box attrs children =
    Html.div
        (Html.Attributes.style "padding" "8px"
            :: Html.Attributes.style "border" "1px solid black"
            :: Html.Attributes.style "border-radius" "8px"
            :: attrs
        )
        children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    Html.div
        (Html.Attributes.style "display" "flex"
            :: gap
            :: attrs
        )
        children


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    Html.div
        (Html.Attributes.style "display" "flex"
            :: Html.Attributes.style "flex-direction" "column"
            :: gap
            :: attrs
        )
        children


wrappedRow : List (Attribute msg) -> List (Html msg) -> Html msg
wrappedRow attrs children =
    Html.div
        (Html.Attributes.style "display" "flex"
            :: Html.Attributes.style "flex-wrap" "wrap"
            :: gap
            :: attrs
        )
        children
