module Main exposing (..)

import Browser
import Data.ClockMode as ClockMode exposing (..)
import Data.Player as Player exposing (Player(..))
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (..)
import Platform.Cmd as Cmd
import Return
import Time
import TypedTime
import Utils exposing (iff)
import View exposing (view)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model.initialModel 3, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.outOfTime /= Nothing || model.paused then
        Sub.none

    else
        Time.every 100 (\_ -> Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTimeControl timeControl ->
            Return.singleton model
                |> Return.map (\m -> { m | clockMode = ClockMode.updateTimeControl timeControl m.clockMode })

        SetDelayCompensation delay ->
            Return.singleton model
                |> Return.map (\m -> { m | clockMode = ClockMode.updateCompensation delay m.clockMode })

        ConfigureTime time ->
            Return.singleton model
                |> Return.map (Model.configureTime time)

        ApplyConfiguration ->
            Return.singleton model
                |> Return.map Model.setTime
                |> Return.map (Model.toggleSettings False)
                |> Return.map (\m -> { m | paused = True, turn = Nothing })

        ShowSettings bool ->
            Return.singleton model
                |> Return.map (\m -> { m | paused = True })
                |> Return.map (Model.toggleSettings bool)

        NoOp ->
            Return.singleton model

        Moved player ->
            Return.singleton model
                |> Return.map Model.timeCompensation
                |> Return.map Model.incrementMoves
                |> Return.map Model.resetPartialTime
                |> Return.map (\m -> { m | paused = iff (model.turn == Nothing) False model.paused })
                |> Return.map (\m -> { m | turn = Maybe.map Player.switch m.turn |> Maybe.or (Just <| Player.switch player) })

        Tick ->
            Return.singleton model
                |> Return.map Model.incrementTime
                |> Return.map Model.verifyOutofTime

        Pause ->
            Return.singleton model
                |> Return.map (\m -> { m | paused = True })

        Resume ->
            Return.singleton model
                |> Return.map (\m -> { m | paused = False })
