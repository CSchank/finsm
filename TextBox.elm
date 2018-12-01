module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)
import Debug exposing (log)
import Katex as K exposing (Latex, human, inline, display, generate)
import Html as H exposing (Html, node)
import Html.Attributes exposing (placeholder)
import Json.Encode
import Debug exposing (log)
import Window
import Tuple exposing (first, second)
import Task
import Html exposing (input)
import Html.Events exposing (onInput)


type Msg
    = Tick Float GetKeyState
    | Change String


main =
    cmdApp Tick
        { init =
            ( "Hello"
            , Cmd.none
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    case msg of
        Tick t _ ->
            ( model, Cmd.none )

        Change str ->
            ( str, Cmd.none )



--List.map (\state -> )


textBox =
    input [ placeholder "Text to reverse", onInput Change ] []


view model =
    collage
        1280
        --winX
        720
        --winY
        [ html <| textBox
        ]
