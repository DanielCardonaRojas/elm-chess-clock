module Data.ClockMode exposing (..)

import TypedTime exposing (TypedTime, Unit(..))


type TimeControl
    = Fischer
    | Delay
    | Bronstein


type alias ClockMode =
    { timeControl : TimeControl
    , compensation : TypedTime
    , duration : TypedTime
    }


updateTimeControl : TimeControl -> ClockMode -> ClockMode
updateTimeControl control mode =
    { mode | timeControl = control }


updateDuration : TypedTime -> ClockMode -> ClockMode
updateDuration time mode =
    { mode | duration = time }


updateCompensation : TypedTime -> ClockMode -> ClockMode
updateCompensation time mode =
    { mode | compensation = time }
