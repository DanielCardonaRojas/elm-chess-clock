module Model exposing (..)

import Data.ClockMode exposing (ClockMode, TimeControl(..))
import Data.Player exposing (Player(..))
import String exposing (fromInt)
import TypedTime exposing (TypedTime, Unit(..), milliseconds, seconds)
import Utils exposing (iff)


type alias Model =
    { turn : Player
    , player1Time : TypedTime -- Total amount of clock ticks accumulated
    , player2Time : TypedTime
    , player1PartialTime : TypedTime -- Amount of clock ticks accumulated until moves
    , player2PartialTime : TypedTime
    , player1MoveCount : Int
    , player2MoveCount : Int
    , clockMode : ClockMode
    , tickLength : TypedTime
    , paused : Bool
    }


initialModel : Int -> Model
initialModel minutes =
    { turn = Player1
    , player1Time = TypedTime.minutes <| toFloat minutes
    , player2Time = TypedTime.minutes <| toFloat minutes
    , player1PartialTime = TypedTime.zero
    , player2PartialTime = TypedTime.zero
    , player1MoveCount = 0
    , player2MoveCount = 0
    , clockMode = { timeControl = Fisher, compensation = TypedTime.seconds 5 }
    , tickLength = TypedTime.milliseconds 100
    , paused = False
    }


remainingTime : Model -> Player -> String
remainingTime model player =
    case player of
        Player1 ->
            TypedTime.toString Seconds model.player1Time

        Player2 ->
            TypedTime.toString Seconds model.player2Time


incrementMoves : Model -> Model
incrementMoves model =
    { model
        | player1MoveCount = iff (model.turn == Player1) (model.player1MoveCount + 1) model.player1MoveCount
        , player2MoveCount = iff (model.turn == Player2) (model.player2MoveCount + 1) model.player2MoveCount
    }


incrementTime : Model -> Model
incrementTime model =
    { model
        | player1Time =
            iff (model.turn == Player1) (TypedTime.sub model.player1Time model.tickLength) model.player1Time
        , player1PartialTime =
            iff (model.turn == Player1) (TypedTime.add model.player1PartialTime model.tickLength) TypedTime.zero
        , player2Time =
            iff (model.turn == Player2) (TypedTime.sub model.player2Time model.tickLength) model.player2Time
        , player2PartialTime =
            iff (model.turn == Player2) (TypedTime.add model.player2PartialTime model.tickLength) TypedTime.zero
    }


timeCompensation : Model -> Model
timeCompensation model =
    let
        compensation =
            case model.clockMode.timeControl of
                Fisher ->
                    TypedTime.add model.clockMode.compensation

                _ ->
                    TypedTime.add TypedTime.zero

        apply player =
            iff (model.turn == player) compensation identity
    in
    { model
        | player1Time = apply Player1 model.player1Time
        , player2Time = apply Player2 model.player2Time
    }



-- remainingTime : Model -> Player -> String
-- remainingTime model player =
--     let
--         convertToString time =
--             if TypedTime.lt time (seconds 30) then
--                 TypedTime.toString Milliseconds time
--             else
--                 TypedTime.toString Milliseconds time
--         calculate ticks partialTicks =
--             TypedTime.multiply (toFloat ticks) model.tickLength
--                 |> TypedTime.sub model.totalTime
--                 |> iff (model.clockMode.timeControl == Fisher) (TypedTime.add model.clockMode.compensation) identity
--                 |> convertToString
--     in
--     case player of
--         Player1 ->
--             calculate model.player1Ticks model.player1PartialTicks
--         Player2 ->
--             calculate model.player2Ticks model.player2PartialTicks
