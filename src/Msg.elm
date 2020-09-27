module Msg exposing (..)

import Data.ClockMode exposing (TimeControl)
import TypedTime exposing (TypedTime)


type Msg
    = SwitchPlayer
    | Pause
    | Resume
    | Tick
    | ShowSettings Bool
      -- Time Control setup
    | SetDelayCompensation TypedTime
    | ConfigureTime TypedTime
    | ApplyConfiguration
    | SetTimeControl TimeControl
    | NoOp
