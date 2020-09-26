module Main exposing (..)

import Browser
import Data.Player as Player exposing (Player(..))
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
    if model.paused then
        Sub.none

    else
        Time.every 100 (\_ -> Tick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SwitchPlayer ->
            Return.singleton model
                |> Return.map Model.timeCompensation
                |> Return.map Model.incrementMoves
                |> Return.map (\m -> { m | turn = Player.switch m.turn })

        Tick ->
            Return.singleton model
                |> Return.map Model.incrementTime

        Pause ->
            Return.singleton model
                |> Return.map (\m -> { m | paused = True })

        Resume ->
            Return.singleton model
                |> Return.map (\m -> { m | paused = False })
