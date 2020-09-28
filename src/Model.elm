module Model exposing (..)

import Data.ClockMode as ClockMode exposing (ClockMode, TimeControl(..))
import Data.Player exposing (Player(..))
import String exposing (fromInt)
import Time.Values as Time exposing (TimeValues)
import TypedTime exposing (TypedTime, Unit(..), milliseconds, seconds)
import Utils exposing (iff)


type alias Model =
    { turn : Maybe Player
    , player1Time : TypedTime -- Total amount of clock ticks accumulated
    , player2Time : TypedTime
    , player1PartialTime : TypedTime -- Amount of clock ticks accumulated until moves
    , player2PartialTime : TypedTime
    , player1MoveCount : Int
    , player2MoveCount : Int
    , outOfTime : Maybe Player
    , clockMode : ClockMode
    , tickLength : TypedTime
    , displaySettings : Bool
    , paused : Bool
    }


initialModel : Int -> Model
initialModel minutes =
    { turn = Nothing
    , player1Time = TypedTime.minutes <| toFloat minutes
    , player2Time = TypedTime.minutes <| toFloat minutes
    , player1PartialTime = TypedTime.zero
    , player2PartialTime = TypedTime.zero
    , player1MoveCount = 0
    , outOfTime = Just Player1
    , displaySettings = True
    , player2MoveCount = 0
    , clockMode =
        { timeControl = Fischer
        , compensation = TypedTime.seconds 5
        , duration = TypedTime.minutes <| toFloat minutes
        }
    , tickLength = TypedTime.milliseconds 100
    , paused = True
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


isPlayerTurn : Player -> Model -> Bool
isPlayerTurn player model =
    model.turn
        |> Maybe.map (\turn -> turn == player)
        |> Maybe.withDefault False


incrementMoves : Model -> Model
incrementMoves model =
    { model
        | player1MoveCount = iff (isPlayerTurn Player1 model) (model.player1MoveCount + 1) model.player1MoveCount
        , player2MoveCount = iff (isPlayerTurn Player2 model) (model.player2MoveCount + 1) model.player2MoveCount
    }


resetPartialTime : Model -> Model
resetPartialTime model =
    { model
        | player1PartialTime = TypedTime.zero
        , player2PartialTime = TypedTime.zero
    }


incrementTime : Model -> Model
incrementTime model =
    { model
        | player1Time =
            iff (isPlayerTurn Player1 model) (TypedTime.sub model.player1Time model.tickLength) model.player1Time
        , player1PartialTime =
            iff (isPlayerTurn Player1 model) (TypedTime.add model.player1PartialTime model.tickLength) model.player1PartialTime
        , player2Time =
            iff (isPlayerTurn Player2 model) (TypedTime.sub model.player2Time model.tickLength) model.player2Time
        , player2PartialTime =
            iff (isPlayerTurn Player2 model) (TypedTime.add model.player2PartialTime model.tickLength) model.player2PartialTime
    }


verifyOutofTime : Model -> Model
verifyOutofTime model =
    { model
        | outOfTime =
            if TypedTime.lte model.player1Time TypedTime.zero then
                Just Player1

            else if TypedTime.lte model.player2Time TypedTime.zero then
                Just Player2

            else
                Nothing
    }


timeCompensation : Model -> Model
timeCompensation model =
    let
        compensation elapsedTime =
            case model.clockMode.timeControl of
                Fischer ->
                    TypedTime.add model.clockMode.compensation

                Bronstein ->
                    elapsedTime
                        |> TypedTime.sub model.clockMode.compensation
                        |> (\diff ->
                                iff (TypedTime.lte diff TypedTime.zero) model.clockMode.compensation elapsedTime
                                    |> TypedTime.add
                           )

                _ ->
                    TypedTime.add TypedTime.zero

        apply player elapsedTime =
            iff (isPlayerTurn player model) (compensation elapsedTime) identity
    in
    { model
        | player1Time = apply Player1 model.player1PartialTime model.player1Time
        , player2Time = apply Player2 model.player2PartialTime model.player2Time
    }


setTime : Model -> Model
setTime model =
    { model
        | player1Time = model.clockMode.duration
        , player2Time = model.clockMode.duration
        , outOfTime = Nothing
    }


configureTime : TypedTime -> Model -> Model
configureTime time model =
    { model
        | clockMode = ClockMode.updateDuration time model.clockMode
    }


toggleSettings : Bool -> Model -> Model
toggleSettings bool m =
    { m | displaySettings = bool }
