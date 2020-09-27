module Msg exposing (..)

import Data.ClockMode exposing (TimeControl)
import Data.Player exposing (Player)
import TypedTime exposing (TypedTime)


type Msg
    = Moved Player
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
