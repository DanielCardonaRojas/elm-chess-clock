module Colors exposing (..)

import Element exposing (Color, rgb, rgb255, rgba)
import Element.HexColor as HexColor
import Html exposing (Attribute)


hex : String -> Color
hex value =
    HexColor.hex value |> Maybe.withDefault (rgb 0 0 0)


caribeanGreen =
    hex "#06D6A0"


tarawera =
    hex "#073B4C"


paradisePink =
    hex "#EF476F"
