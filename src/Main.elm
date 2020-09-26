module Main exposing (..)

import Browser
import Data.Player as Player exposing (Player(..))
import Model exposing (..)
import Msg exposing (..)
import Platform.Cmd as Cmd
import Time
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
    ( Model.initialModel, Cmd.none )


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
            ( { model | turn = Player.switch model.turn }, Cmd.none )

        Tick ->
            ( { model
                | player1Ticks =
                    if model.turn == Player1 then
                        model.player1Ticks + 1

                    else
                        model.player1Ticks
                , player2Ticks =
                    if model.turn == Player2 then
                        model.player2Ticks + 1

                    else
                        model.player2Ticks
              }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )
