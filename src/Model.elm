module Model exposing (..)

import Data.Player exposing (Player(..))
import TypedTime exposing (TypedTime, Unit(..), milliseconds, seconds)


type alias Model =
    { turn : Player
    , player1Ticks : Int
    , player2Ticks : Int
    , clockMode : ClockMode
    , tickLength : TypedTime
    , totalTime : TypedTime
    }


type ClockMode
    = ClockMode


remainingTime : Model -> Player -> String
remainingTime model player =
    let
        convertToString time =
            if TypedTime.lt time (seconds 30) then
                TypedTime.toString Milliseconds time

            else
                TypedTime.toString Milliseconds time

        calculate ticks =
            TypedTime.multiply (toFloat ticks) model.tickLength
                |> TypedTime.sub model.totalTime
                |> convertToString
    in
    case player of
        Player1 ->
            calculate model.player1Ticks

        Player2 ->
            calculate model.player2Ticks
