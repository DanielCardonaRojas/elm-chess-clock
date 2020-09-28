module View exposing (..)

import Colors
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
        [ inFront <| iff model.displaySettings (settingsUI model) none
        , Font.color Colors.tarawera
        , Font.size (scaled 1)
        ]
    <|
        applicationUI model


applicationUI : Model -> Element Msg
applicationUI model =
    el [ width fill, height fill, padding 20 ] <|
        column [ width fill, height fill, spacing 10 ]
            [ playerTimeTile Player2 model
            , optionSection model
            , playerTimeTile Player1 model
            ]


playerTimeTile : Player -> Model -> Element Msg
playerTimeTile player model =
    let
        canTapTile p =
            (model.turn == Nothing)
                || (Model.isPlayerTurn p model && not model.paused)

        { isActive, canTap, ranOutOfTime, rotated, remainingTime, moves } =
            { isActive = Model.isPlayerTurn player model
            , canTap = canTapTile player
            , ranOutOfTime = Maybe.map (\p -> p == player) model.outOfTime |> Maybe.withDefault False
            , rotated = player == Player2
            , remainingTime = Model.remainingTime model player
            , moves = model.player2MoveCount
            }
    in
    Input.button
        [ centerX
        , rotate <| iff rotated 3.14156 0
        , height <| fillPortion 2
        , width fill
        , Border.rounded 8
        , Element.inFront <|
            el
                [ alignBottom
                , alignRight
                , padding 30
                , Font.size (scaled 2)
                , Font.bold
                ]
                (text <| "Moves: " ++ String.fromInt moves)
        , Colors.tarawera
            |> iff isActive Colors.caribeanGreen
            >> iff ranOutOfTime Colors.paradisePink
            |> Background.color
        , Font.color <| rgb 1 1 1
        ]
        { onPress = iff canTap (Just <| Moved player) Nothing
        , label = el [ centerX, centerY, Font.size (scaled 6) ] (text remainingTime)
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


settingsUI : Model -> Element Msg
settingsUI model =
    el
        [ Background.color <| withAlpha 0.85 <| Colors.tarawera
        , width fill
        , height fill
        , padding 64
        ]
    <|
        el
            [ Background.color <| rgb255 255 255 255
            , centerX
            , centerY
            , Border.rounded 8
            ]
        <|
            column
                [ centerX
                , centerY
                , height fill
                , spaceEvenly
                , spacing 20
                , padding 32
                ]
                [ image
                    [ Event.onClick <| ShowSettings False
                    , width <| px 30
                    , moveLeft 15
                    , moveUp 15
                    ]
                    { src = "../assets/x-mark.png"
                    , description = "close button"
                    }
                , el [ centerX, Font.extraBold, Font.size (scaled 3) ] <| text "Settings"
                , Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = SetTimeControl
                    , selected = Just model.clockMode.timeControl
                    , label = Input.labelAbove [ centerX ] (text "Time Control")
                    , options =
                        [ Input.option Fischer (el [ Font.size (scaled -2) ] <| text "Fischer")
                        , Input.option Bronstein (el [ Font.size (scaled -2) ] <| text "Bronstein")
                        , Input.option Delay (el [ Font.size (scaled -2) ] <| text "Delay")
                        ]
                    }
                , Input.slider
                    [ Element.height (Element.px 30)
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Background.color Colors.tarawera
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
                            , Background.color Colors.tarawera
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
                    , Background.color Colors.caribeanGreen
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
        [ iff (model.turn == Nothing) none (pauseResumeButton <| not model.paused)
        , settingsButton
        ]


scaled =
    Element.modular 16 1.25 >> round


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    color
        |> toRgb
        |> (\c -> { c | alpha = alpha })
        |> fromRgb
