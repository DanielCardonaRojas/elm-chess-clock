module Model exposing (..)

import Data.ClockMode exposing (ClockMode, TimeControl(..))
import Data.Player exposing (Player(..))
import TypedTime exposing (TypedTime, Unit(..), milliseconds, seconds)
import Utils exposing (iff)


type alias Model =
    { turn : Player
    , player1Ticks : Int -- Total amount of clock ticks accumulated
    , player2Ticks : Int
    , player1PartialTicks : Int -- Amount of clock ticks accumulated until moves
    , player2PartialTicks : Int
    , player1MoveCount : Int
    , player2MoveCount : Int
    , clockMode : ClockMode
    , tickLength : TypedTime
    , totalTime : TypedTime
    , paused : Bool
    }


initialModel : Model
initialModel =
    { turn = Player1
    , player1Ticks = 0
    , player2Ticks = 0
    , player1PartialTicks = 0
    , player2PartialTicks = 0
    , player1MoveCount = 0
    , player2MoveCount = 0
    , clockMode = { timeControl = Fisher, compensation = TypedTime.seconds 5 }
    , tickLength = TypedTime.milliseconds 100
    , totalTime = TypedTime.seconds 40
    , paused = False
    }


remainingTime : Model -> Player -> String
remainingTime model player =
    let
        convertToString time =
            if TypedTime.lt time (seconds 30) then
                TypedTime.toString Milliseconds time

            else
                TypedTime.toString Milliseconds time

        calculate ticks partialTicks =
            TypedTime.multiply (toFloat ticks) model.tickLength
                |> TypedTime.sub model.totalTime
                |> iff (model.clockMode.timeControl == Fisher) (TypedTime.add model.clockMode.compensation) identity
                |> convertToString
    in
    case player of
        Player1 ->
            calculate model.player1Ticks model.player1PartialTicks

        Player2 ->
            calculate model.player2Ticks model.player2PartialTicks
