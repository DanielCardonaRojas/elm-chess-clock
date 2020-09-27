module View exposing (..)

import Data.ClockMode exposing (TimeControl(..))
import Data.Player as Player exposing (Player(..))
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Model exposing (..)
import Msg exposing (..)
import TypedTime exposing (TypedTime, minutes, seconds)
import Utils exposing (iff)


view model =
    Element.layoutWith
        { options =
            [ noHover
            , Element.focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing }
            ]
        }
        [ inFront <| iff model.displaySettings (settingsUI model) none ]
    <|
        applicationUI model


applicationUI : Model -> Element Msg
applicationUI model =
    let
        canTapTile player =
            model.turn == player && not model.paused
    in
    el [ width fill, height fill, padding 20 ] <|
        column [ width fill, height fill, spacing 10 ]
            [ playerTimeTile
                { isActive = canTapTile Player2
                , rotated = True
                , remainingTime = Model.remainingTime model Player2
                , moves = model.player2MoveCount
                }
            , optionSection model
            , playerTimeTile
                { isActive = canTapTile Player1
                , rotated = False
                , remainingTime = Model.remainingTime model Player1
                , moves = model.player1MoveCount
                }
            ]


settingsUI : Model -> Element Msg
settingsUI model =
    el
        [ Background.color <| rgba255 100 100 100 0.8
        , width fill
        , height fill
        ]
    <|
        el
            [ Background.color <| rgb255 255 255 255
            , centerX
            , centerY
            , fill |> minimum 200 |> maximum 400 |> width
            , padding 32
            , shrink |> minimum 200 |> height
            , Border.rounded 8
            ]
        <|
            column
                [ centerX
                , centerY
                , height fill
                , spaceEvenly
                , spacing 20
                ]
                [ el [ centerX, Font.extraBold ] <| text "Settings"
                , Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = SetTimeControl
                    , selected = Just model.clockMode.timeControl
                    , label = Input.labelAbove [ centerX ] (text "Time Control")
                    , options =
                        [ Input.option Fisher (text "Fisher")
                        , Input.option Bronstein (text "Bronstein")
                        , Input.option Delay (text "Delay")
                        ]
                    }
                , Input.slider
                    [ Element.height (Element.px 30)
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Background.color <| rgb255 100 100 100
                            , Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { onChange = TypedTime.seconds >> SetDelayCompensation
                    , label =
                        Input.labelAbove []
                            (text <| (\s -> s ++ " seconds") <| String.fromInt <| .seconds <| Model.clockDelayConfiguration model)
                    , min = 0
                    , max = 10
                    , step = Just 1
                    , value = Model.clockDelayConfiguration model |> .seconds |> toFloat
                    , thumb = Input.defaultThumb
                    }
                , Input.slider
                    [ Element.height (Element.px 30)
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Background.color <| rgb255 100 100 100
                            , Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { onChange = TypedTime.minutes >> ConfigureTime
                    , label =
                        Input.labelAbove []
                            (text <| (\s -> s ++ " min") <| String.fromInt <| .minutes <| Model.clockTimeConfiguration model)
                    , min = 0
                    , max = 30
                    , step = Just 1
                    , value = Model.clockConfigurationMinutes model
                    , thumb = Input.defaultThumb
                    }
                , Input.button
                    [ centerX
                    , Background.color <| rgb255 180 50 60
                    , Border.rounded 8
                    , paddingXY 80 10
                    , Font.color <| rgb255 255 255 255
                    ]
                    { onPress = Just ApplyConfiguration
                    , label = text "Start"
                    }
                ]



-- Sections and widgets


optionSection : Model -> Element Msg
optionSection model =
    row [ centerX, spacing 20 ]
        [ pauseResumeButton <| not model.paused
        , settingsButton
        ]


playerTimeTile : { isActive : Bool, rotated : Bool, remainingTime : String, moves : Int } -> Element Msg
playerTimeTile { isActive, rotated, remainingTime, moves } =
    Input.button
        [ Event.onClick <| iff isActive SwitchPlayer NoOp
        , centerX
        , rotate <| iff rotated 3.14156 0
        , height <| fillPortion 2
        , width fill
        , Border.rounded 8
        , Element.inFront <| el [ alignBottom, alignRight, padding 30 ] (text <| "Moves: " ++ String.fromInt moves)
        , Background.color <| iff isActive (rgb255 187 120 38) (rgb 0.1 0.1 0.1)
        , Font.color <| rgb 1 1 1
        ]
        { onPress = iff isActive (Just SwitchPlayer) Nothing
        , label = el [ centerX, centerY, Font.size 48 ] (text remainingTime)
        }


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


settingsButton : Element Msg
settingsButton =
    image
        [ Event.onClick <| ShowSettings True
        , width <| px 50
        ]
        { src = "../assets/gear.png"
        , description = "Settings button"
        }
