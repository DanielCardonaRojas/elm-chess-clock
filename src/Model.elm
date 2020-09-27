module Model exposing (..)

import Data.ClockMode as ClockMode exposing (ClockMode, TimeControl(..))
import Data.Player exposing (Player(..))
import String exposing (fromInt)
import Time.Values as Time exposing (TimeValues)
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
    , displaySettings : Bool
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
    , displaySettings = False
    , player2MoveCount = 0
    , clockMode =
        { timeControl = Fisher
        , compensation = TypedTime.seconds 5
        , duration = TypedTime.minutes <| toFloat minutes
        }
    , tickLength = TypedTime.milliseconds 100
    , paused = False
    }



-- Computed properties


clockConfigurationMinutes : Model -> Float
clockConfigurationMinutes model =
    TypedTime.toSeconds model.clockMode.duration |> round |> Time.fromSeconds |> .minutes |> toFloat


clockTimeConfiguration : Model -> TimeValues
clockTimeConfiguration model =
    TypedTime.toSeconds model.clockMode.duration |> round |> Time.fromSeconds


clockDelayConfiguration : Model -> TimeValues
clockDelayConfiguration model =
    TypedTime.toSeconds model.clockMode.compensation |> round |> Time.fromSeconds



-- Updates


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


setTime : Model -> Model
setTime model =
    { model | player1Time = model.clockMode.duration, player2Time = model.clockMode.duration }


configureTime : TypedTime -> Model -> Model
configureTime time model =
    { model
        | clockMode = ClockMode.updateDuration time model.clockMode
    }


toggleSettings : Bool -> Model -> Model
toggleSettings bool m =
    { m | displaySettings = bool }
