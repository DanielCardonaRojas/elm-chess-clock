module View exposing (..)

import Data.Player as Player exposing (Player(..))
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Events as Event
import Element.Font as Font
import Model exposing (..)
import Msg exposing (..)


view model =
    Element.layout [] <| applicationUI model


applicationUI : Model -> Element Msg
applicationUI model =
    el [ width fill, height fill, Element.explain Debug.todo ] <|
        column [ width fill, height fill, padding 10, spacing 8 ]
            [ renderTimeTile (model.turn == Player1) (Model.remainingTime model Player1)
            , renderTimeTile (model.turn == Player2) (Model.remainingTime model Player2)
            ]


renderTimeTile : Bool -> String -> Element Msg
renderTimeTile isActive remainingTime =
    el
        [ Event.onClick SwitchPlayer
        , centerX
        , height <| fillPortion 2
        , width fill
        , Background.color <|
            if isActive then
                rgb255 187 120 38

            else
                rgb 0.1 0.1 0.1
        , Font.color <| rgb 1 1 1
        ]
    <|
        el [ centerX, centerY ] (text remainingTime)
