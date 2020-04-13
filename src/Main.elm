port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Element as Element
    exposing
        ( Color
        , Device
        , Element
        , alignLeft
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , maximum
        , minimum
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes exposing (class, id, title)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform.Sub as Sub
import Task as Task
import Time as Time


type alias Model =
    { timer : Timer
    , device : Device
    }


type Timer
    = Stopped
    | Running TimerData
    | Paused Ms
    | Finished


type alias TimerData =
    { startTime : Ms -- when current countdown has started
    , countdown : Ms -- what is the current countdown value (const when running)
    , currentTime : Ms
    }


type alias Flags =
    { width : Int
    , height : Int
    }


type Msg
    = Start
    | Pause
    | Reset
    | Tick Ms
    | Resize Int Int
    | NoOp


type Ms
    = Ms Int


port saveTimerModel : Encode.Value -> Cmd msg


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- 25 min * 60 s * 1000 ms


elmodoro : Int
elmodoro =
    25 * 60 * tick


tick : Int
tick =
    1000


init : Decode.Value -> ( Model, Cmd Msg )
init json =
    let
        flags =
            decodeFlagsWithDefaults json

        timer =
            decodeTimer json
                |> (\t ->
                        if isValid t then
                            t

                        else
                            Stopped
                   )

        initCmd =
            case timer of
                Running _ ->
                    nowTickCmd

                _ ->
                    Cmd.none
    in
    ( { timer = timer
      , device = Element.classifyDevice flags
      }
    , initCmd
    )


isValid : Timer -> Bool
isValid timer =
    case timer of
        Stopped ->
            True

        Finished ->
            True

        Paused ms ->
            isValidCountdown ms

        Running data ->
            let
                (Ms startTime) =
                    data.startTime

                (Ms countdown) =
                    data.countdown

                (Ms currentTime) =
                    data.currentTime
            in
            currentTime
                >= startTime
                && isValidCountdown (Ms (currentTime - startTime))
                && isValidCountdown data.countdown


isValidCountdown : Ms -> Bool
isValidCountdown (Ms ms) =
    0 < ms && ms <= elmodoro


decodeFlagsWithDefaults : Decode.Value -> Flags
decodeFlagsWithDefaults json =
    Decode.decodeValue flagsDecoder json
        |> Result.withDefault (Flags 360 640)


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map2 Flags
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


encodeTimer : Timer -> Encode.Value
encodeTimer timer =
    Encode.string <|
        Encode.encode 0 <|
            case timer of
                Stopped ->
                    Encode.object
                        [ ( "tag", Encode.string "Stopped" ) ]

                Paused (Ms ms) ->
                    Encode.object
                        [ ( "tag", Encode.string "Paused" )
                        , ( "countdown", Encode.int ms )
                        ]

                Finished ->
                    Encode.object
                        [ ( "tag", Encode.string "Finished" ) ]

                Running data ->
                    let
                        (Ms startTime) =
                            data.startTime

                        (Ms countdown) =
                            data.countdown

                        (Ms currentTime) =
                            data.currentTime
                    in
                    Encode.object
                        [ ( "tag", Encode.string "Running" )
                        , ( "startTime", Encode.int startTime )
                        , ( "countdown", Encode.int countdown )
                        , ( "currentTime", Encode.int currentTime )
                        ]


decodeTimer : Decode.Value -> Timer
decodeTimer =
    let
        timerDecoder =
            Decode.field "tag" Decode.string
                |> Decode.andThen
                    (\tag ->
                        case tag of
                            "Stopped" ->
                                Decode.succeed Stopped

                            "Finished" ->
                                Decode.succeed Finished

                            "Paused" ->
                                Decode.field "countdown" Decode.int
                                    |> Decode.andThen
                                        (\ms ->
                                            Decode.succeed (Paused <| Ms ms)
                                        )

                            "Running" ->
                                Decode.map3 TimerData
                                    (Decode.field "startTime" Decode.int |> Decode.map Ms)
                                    (Decode.field "countdown" Decode.int |> Decode.map Ms)
                                    (Decode.field "currentTime" Decode.int |> Decode.map Ms)
                                    |> Decode.map Running

                            _ ->
                                Decode.succeed Stopped
                    )
    in
    Result.withDefault Stopped
        << Decode.decodeString timerDecoder
        << Result.withDefault ""
        -- value should be a string stored in the localStorage
        << Decode.decodeValue (Decode.field "timer" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        resizeSub =
            Events.onResize (\w h -> Resize w h)
    in
    case model.timer of
        Running _ ->
            Sub.batch
                [ Time.every
                    (toFloat tick)
                    (Tick << Ms << Time.posixToMillis)
                , resizeSub
                ]

        _ ->
            resizeSub


nowTickCmd : Cmd Msg
nowTickCmd =
    Task.perform
        (Tick << Ms << Time.posixToMillis)
        Time.now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            case model.timer of
                Stopped ->
                    ( { model | timer = Stopped }, nowTickCmd )

                Finished ->
                    ( { model | timer = Stopped }, nowTickCmd )

                Paused ms ->
                    ( { model | timer = Paused ms }, nowTickCmd )

                _ ->
                    ( model, Cmd.none )

        Pause ->
            case model.timer of
                Running timer ->
                    let
                        newTimer =
                            Paused <| calculateNewCountdown timer
                    in
                    ( { model | timer = newTimer }
                    , saveTimerModel <| encodeTimer newTimer
                    )

                _ ->
                    ( model, Cmd.none )

        Reset ->
            case model.timer of
                Paused ms ->
                    let
                        newTimer =
                            Stopped
                    in
                    ( { model | timer = newTimer }
                    , Cmd.batch
                        [ saveTimerModel <| encodeTimer newTimer
                        , focusStartButton
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Tick now ->
            let
                newTimer =
                    case model.timer of
                        -- Start button was clicked in prev step
                        -- now Task has been fulfilled
                        Stopped ->
                            Running
                                { startTime = now
                                , countdown = Ms elmodoro
                                , currentTime = now
                                }

                        -- Contine button was clicked in prev step
                        -- now Task has been fulfilled
                        Paused lastCountdown ->
                            Running
                                { startTime = now
                                , countdown = lastCountdown
                                , currentTime = now
                                }

                        Running timer ->
                            tickTimer timer now

                        _ ->
                            model.timer

                cmd =
                    if model.timer == newTimer then
                        Cmd.none

                    else if newTimer == Finished then
                        Cmd.batch
                            [ saveTimerModel <|
                                encodeTimer Stopped
                            , focusStartButton
                            ]

                    else
                        saveTimerModel <|
                            encodeTimer newTimer
            in
            ( { model | timer = newTimer }, cmd )

        Resize w h ->
            ( { model
                | device =
                    Element.classifyDevice
                        { width = w
                        , height = h
                        }
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


focusStartButton : Cmd Msg
focusStartButton =
    Task.attempt (\_ -> NoOp) (Dom.focus timerButtonId)


calculateNewCountdown : TimerData -> Ms
calculateNewCountdown timer =
    let
        (Ms startTime) =
            timer.startTime

        (Ms countdown) =
            timer.countdown

        (Ms currentTime) =
            timer.currentTime
    in
    Ms <| countdown - roundToTick (currentTime - startTime)


tickTimer : TimerData -> Ms -> Timer
tickTimer timer (Ms now) =
    let
        (Ms startTime) =
            timer.startTime

        (Ms countdown) =
            timer.countdown

        countdownNow =
            roundToTick (now - startTime)
    in
    if countdownNow < 0 then
        Stopped

    else if countdownNow < countdown then
        Running { timer | currentTime = Ms now }

    else
        Finished


roundToTick : Int -> Int
roundToTick ms =
    ms // tick * tick


white : Color
white =
    rgb255 0xFE 0xFE 0xFE


black : Color
black =
    rgb255 0x11 0x11 0x11


grey : Color
grey =
    rgb255 0x5A 0x63 0x78


blue : Color
blue =
    rgb255 0x60 0xB5 0xCC


yellow : Color
yellow =
    rgb255 0xF0 0xAD 0x00


green : Color
green =
    rgb255 0x7F 0xD1 0x3B


red : Color
red =
    rgb255 0xEE 0x00 0x00


boxMaxWidth : Int
boxMaxWidth =
    380


view : Model -> Html Msg
view model =
    let
        isMobile =
            Element.Phone == model.device.class

        horizontalAlignment =
            if isMobile then
                alignLeft

            else
                centerX

        mediaBoxStyles =
            if isMobile then
                [ width fill
                , paddingXY 5 15
                ]

            else
                [ centerY
                , height (px 375)
                , width (px boxMaxWidth)
                , Border.color grey
                , Border.rounded buttonRadius
                , Border.width 1
                ]
    in
    Element.layout [] <|
        el
            [ height fill
            , horizontalAlignment
            , centerY
            , width (fill |> minimum 285)
            , Background.color white
            ]
        <|
            column
                ([ centerX
                 , Font.center
                 , Font.color black
                 , Font.family
                    [ Font.typeface "Open Sans"
                    , Font.typeface "Nunito Sans"
                    , Font.typeface "Source Sans Pro"
                    , Font.typeface "Muli"
                    , Font.typeface "Helvetica"
                    , Font.typeface "Helvetica Neue"
                    , Font.typeface "Ubuntu"
                    , Font.typeface "Noto"
                    , Font.typeface "Segoe UI"
                    , Font.typeface "Arial"
                    , Font.sansSerif
                    ]
                 , Font.size 24
                 , Background.color white
                 ]
                    ++ mediaBoxStyles
                )
                [ wrappedRow
                    [ width fill
                    , centerY
                    , paddingXY 0 20
                    ]
                    [ header ]
                , row
                    [ width fill
                    , centerY
                    , paddingXY 0 20
                    ]
                    [ viewTimer model.timer ]
                , row
                    [ width fill
                    , centerY
                    , paddingXY 0 10
                    ]
                    [ timerButton model.timer ]
                , row
                    [ width fill
                    , centerY
                    , paddingXY 0 5
                    ]
                    [ resetButton model.timer ]
                ]


header : Element Msg
header =
    el
        [ Font.size 52
        , Font.letterSpacing 5
        , width fill
        , alignLeft
        ]
        (text "Elmodoro")


viewTimer : Timer -> Element Msg
viewTimer timer =
    let
        ( timerStr, timerColour ) =
            case timer of
                Stopped ->
                    ( countdownToString <| Ms elmodoro
                    , grey
                    )

                Finished ->
                    ( countdownToString <| Ms 0
                    , grey
                    )

                Paused countdown ->
                    ( countdownToString countdown
                    , grey
                    )

                Running data ->
                    let
                        (Ms startTime) =
                            data.startTime

                        (Ms countdown) =
                            data.countdown

                        (Ms currentTime) =
                            data.currentTime

                        ms =
                            countdown - roundToTick (currentTime - startTime)
                    in
                    ( countdownToString <| Ms ms
                    , black
                    )

        classes =
            if timer == Finished then
                [ htmlAttribute <| class "timer" ]

            else
                []
    in
    paragraph
        ([ Font.color timerColour
         , Font.letterSpacing 2
         , Font.size 36
         ]
            ++ classes
        )
        [ text timerStr ]


countdownToString : Ms -> String
countdownToString (Ms ms) =
    let
        min =
            ms // 1000 // 60

        sec =
            modBy 60 (ms // 1000)
    in
    String.pad 2 '0' (String.fromInt min)
        ++ ":"
        ++ String.pad 2 '0' (String.fromInt sec)


buttonRadius : Int
buttonRadius =
    4


timerButtonId : String
timerButtonId =
    "timer-button"


timerButton : Timer -> Element Msg
timerButton timer =
    let
        button bgColor bdColor msg label =
            Input.button
                [ Background.color white
                , Border.color bgColor
                , Border.rounded buttonRadius
                , Border.width 1
                , Element.focused
                    [ Border.innerGlow bgColor 1.0 ]
                , htmlAttribute <|
                    id timerButtonId
                , Font.color bgColor
                , Font.letterSpacing 1
                , Font.size 22
                , centerX
                , paddingXY 40 5
                , spacing 2
                , width (fill |> maximum 200)
                ]
                { onPress = Just msg
                , label = text label
                }
    in
    case timer of
        Stopped ->
            button blue grey Start "Start"

        Running _ ->
            button yellow grey Pause "Pause"

        Paused _ ->
            button green grey Start "Continue"

        Finished ->
            button blue grey Start "Start"


resetButton : Timer -> Element Msg
resetButton timer =
    let
        disabledDesc =
            "Reset button only works when the timer is paused."

        statusStyles =
            case timer of
                Paused _ ->
                    [ Element.focused
                        [ Border.color red ]
                    , Font.color red
                    , Font.underline
                    ]

                _ ->
                    [ Element.focused
                        [ Border.color grey ]
                    , Font.color grey
                    , Region.description disabledDesc
                    , htmlAttribute <|
                        title disabledDesc
                    ]

        msg =
            case timer of
                Paused _ ->
                    Reset

                _ ->
                    NoOp
    in
    Input.button
        ([ Border.color white
         , Border.dotted
         , Border.rounded buttonRadius
         , Border.width 1
         , Font.size 18
         , centerX
         , paddingXY 10 2
         ]
            ++ statusStyles
        )
        { onPress = Just msg
        , label = text "Reset"
        }
