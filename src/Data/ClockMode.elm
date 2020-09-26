module Data.ClockMode exposing (..)

import TypedTime exposing (TypedTime, Unit(..))


type TimeControl
    = Fisher
    | Delay
    | Bronstein


type alias ClockMode =
    { timeControl : TimeControl
    , compensation : TypedTime
    }
