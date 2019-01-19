module Main exposing (Msg(..), main, textBox, update, view)

import Array exposing (Array)
import Dict exposing (Dict)
import GraphicSVG exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Json.Encode
import Katex as K exposing (Latex, display, generate, human, inline)
import List
import Random
import Set exposing (Set)
import Task
import Tuple exposing (first, second)
import Window


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
