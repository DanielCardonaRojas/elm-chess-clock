module View exposing (..)

import Data.Player as Player exposing (Player(..))
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Model exposing (..)
import Msg exposing (..)


iff : Bool -> a -> a -> a
iff condition trueValue falseValue =
    if condition then
        trueValue

    else
        falseValue


view model =
    Element.layout [] <| applicationUI model


applicationUI : Model -> Element Msg
applicationUI model =
    let
        canTapTile player =
            model.turn == player && not model.paused
    in
    el [ width fill, height fill ] <|
        column [ width fill, height fill, padding 10, spacing 8 ]
            [ renderTimeTile (canTapTile Player1) (Model.remainingTime model Player1)
            , pauseResumeButton <| not model.paused
            , renderTimeTile (canTapTile Player2) (Model.remainingTime model Player2)
            ]


renderTimeTile : Bool -> String -> Element Msg
renderTimeTile isActive remainingTime =
    el
        [ Event.onClick <| iff isActive SwitchPlayer NoOp
        , centerX
        , height <| fillPortion 2
        , width fill
        , Border.rounded 8
        , Background.color <|
            if isActive then
                rgb255 187 120 38

            else
                rgb 0.1 0.1 0.1
        , Font.color <| rgb 1 1 1
        ]
    <|
        el [ centerX, centerY ] (text remainingTime)


pauseResumeButton : Bool -> Element Msg
pauseResumeButton counting =
    el [ centerX ] <|
        image
            [ Event.onClick <| iff counting Pause Resume
            , width <| px 50
            ]
            { src = iff counting "../assets/pause.png" "../assets/play.png"
            , description = "Pause button"
            }
