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
import Utils exposing (iff)



view model =
    Element.layout [] <| applicationUI model


applicationUI : Model -> Element Msg
applicationUI model =
    let
        canTapTile player =
            model.turn == player && not model.paused
    in
    el [ width fill, height fill, padding 20] <|
        column [ width fill, height fill, spacing 10]
            [ renderTimeTile { isActive = canTapTile Player2 
                , rotated = True 
                , remainingTime = Model.remainingTime model Player2 
                , moves = model.player2MoveCount }  
            , pauseResumeButton <| not model.paused
            , renderTimeTile { isActive = canTapTile Player1
                , rotated = False
                , remainingTime = Model.remainingTime model Player1
                , moves = model.player1MoveCount
            }
            ]


renderTimeTile : {isActive: Bool, rotated: Bool, remainingTime: String, moves: Int}  -> Element Msg
renderTimeTile {isActive, rotated, remainingTime, moves } =
    el
        [ Event.onClick <| iff isActive SwitchPlayer NoOp
        -- , Element.explain Debug.todo
        , centerX
        , rotate <| iff rotated 3.14156 0
        , height <| fillPortion 2
        , width fill
        , Border.rounded 8
        , Element.inFront <| el [alignBottom, alignRight, padding 30] (text <| "Moves: " ++ String.fromInt moves)
        , Background.color <| iff isActive (rgb255 187 120 38) (rgb 0.1 0.1 0.1)
        , Font.color <| rgb 1 1 1
        ]
    <|
        el [ centerX, centerY, Font.size 48] (text remainingTime)
        


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
