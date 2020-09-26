module Main exposing (..)

import Browser
import Data.Player as Player exposing (Player(..))
import Model exposing (..)
import Msg exposing (..)
import Platform.Cmd as Cmd
import Time
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
            ( { model
                | turn = Player.switch model.turn
                , player1MoveCount = iff (model.turn == Player1) (model.player1MoveCount + 1) model.player1MoveCount
                , player2MoveCount = iff (model.turn == Player2) (model.player2MoveCount + 1) model.player2MoveCount
              }
            , Cmd.none
            )

        Tick ->
            ( { model
                | player1Ticks =
                    iff (model.turn == Player1) (model.player1Ticks + 1) model.player1Ticks
                , player1PartialTicks =
                    iff (model.turn == Player1) (model.player1PartialTicks + 1) 0
                , player2Ticks =
                    iff (model.turn == Player2) (model.player2Ticks + 1) model.player2Ticks
                , player2PartialTicks =
                    iff (model.turn == Player2) (model.player2PartialTicks + 1) 0
              }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        Resume ->
            ( { model | paused = False }, Cmd.none )
