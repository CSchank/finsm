module Exporting exposing (InputTape, Model(..), Msg(..), Output(..), PersistentModel, exportButton, exportTikz, generateTikz, indtBy, initPModel, onEnter, onExit, output, subscriptions, unlines, update, view)

import Array exposing (Array)
import Browser.Events
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Machine exposing (..)
import Set exposing (Set)
import Sha256 exposing (sha256)
import SharedModel exposing (SharedModel)
import Task
import Time
import Tuple exposing (first, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias PersistentModel =
    { outputType : Output
    , time : Int
    }


type alias InputTape =
    Array Character


type Model
    = Default
    | ShowingOutput


type Output
    = Tikz


type Msg
    = SelectOutput Output
    | GenerateOutput
    | CloseOutput
    | MachineMsg Machine.Msg
    | GetTime Int


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default, pModel, sModel ), False, Task.perform (GetTime << Time.posixToMillis) Time.now )


onExit : Environment -> ( Model, PersistentModel, SharedModel ) -> ( ( PersistentModel, SharedModel ), Bool )
onExit env ( model, pModel, sModel ) =
    ( ( pModel, sModel ), False )


initPModel : PersistentModel
initPModel =
    { outputType = Tikz
    , time = 0
    }


update : Environment -> Msg -> ( Model, PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
update env msg ( model, pModel, sModel ) =
    let
        machine =
            sModel.machine
    in
    case msg of
        SelectOutput outputType ->
            ( ( model, { pModel | outputType = outputType }, sModel ), False, Cmd.none )

        GenerateOutput ->
            ( ( ShowingOutput, pModel, sModel ), False, Cmd.none )

        CloseOutput ->
            ( ( Default, pModel, sModel ), False, Cmd.none )

        MachineMsg mmsg ->
            ( ( model, pModel, sModel ), False, Cmd.none )

        GetTime t ->
            ( ( model, { pModel | time = t }, sModel ), False, Cmd.none )


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine

        winX =
            toFloat <| first env.windowSize

        winY =
            toFloat <| second env.windowSize

        menu =
            group <|
                []
    in
    group
        [ (GraphicSVG.map MachineMsg <| Machine.view env Regular sModel.machine sModel.machine.start) |> move ( -winX / 6, 0 )
        , text "Choose format:"
            |> size 20
            |> fixedwidth
            |> filled black
            |> move ( winX / 6 - 125, 80 )
        , exportTikz (pModel.outputType == Tikz) |> move ( winX / 6, 0 )
        , exportButton
            |> move ( winX / 6, -100 )
            |> notifyTap GenerateOutput
        , case ( model, pModel.outputType ) of
            ( ShowingOutput, Tikz ) ->
                output (winX / 2) (winY / 2) (generateTikz pModel.time sModel.machine)

            _ ->
                group []
        ]


exportTikz : Bool -> Shape Msg
exportTikz selected =
    group
        [ roundedRect 250 75 5
            |> outlined (solid 2) darkGray
        , text "TikZ"
            |> size 20
            |> fixedwidth
            |> filled black
            |> move ( -50, 7.5 )
        , roundedRect 30 15 2
            |> filled finsmBlue
            |> move ( 20, 12.5 )
        , text "Beta"
            |> fixedwidth
            |> size 10
            |> centered
            |> filled white
            |> move ( 20, 9.5 )
        , text "Export code to include"
            |> size 12
            |> fixedwidth
            |> filled black
            |> move ( -50, -10 )
        , text "in a LaTeX document"
            |> size 12
            |> fixedwidth
            |> filled black
            |> move ( -50, -22.5 )
        , circle 10
            |> outlined (solid 1) gray
            |> move ( -90, 0 )
        , circle 8
            |> filled finsmBlue
            |> move ( -90, 0 )
        ]


exportButton =
    group
        [ roundedRect 130 40 5
            |> filled finsmBlue
        , text "Export"
            |> fixedwidth
            |> size 24
            |> centered
            |> filled white
            |> move ( 0, -7 )
        ]


output w h txt =
    group
        [ roundedRect (w + 20) (h + 20) 5
            |> filled white
            |> addOutline (solid 1) gray
        , text "Select all and copy this code into your favourite LaTeX editor"
            |> fixedwidth
            |> size 8
            |> filled black
            |> move ( -w / 2, h / 2 - 5 )
        , (html w (h - 10) <|
            H.div
                [ A.style "width" "100%"
                , A.style "height" "100%"
                , A.style "padding" "2px"
                , A.style "padding-right" "4px"
                ]
                [ H.textarea
                    [ A.value txt
                    , A.style "width" "99%"
                    , A.style "height" "98%"
                    , A.style "border" "none"
                    , A.style "resize" "none"
                    , A.style "border-radius" "2px"
                    , A.style "position" "fixed"
                    , A.readonly True
                    ]
                    []
                ]
          )
            |> move ( -w / 2, h / 2 - 12.5 )
        , group
            [ circle 10
                |> filled white
                |> addOutline (solid 2) gray
            , roundedRect 10 3 1.5 |> filled gray
            , roundedRect 3 10 1.5 |> filled gray
            ]
            |> rotate (degrees 45)
            |> notifyTap CloseOutput
            |> move ( w / 2 - 5, h / 2 - 5 )
        ]


generateTikz : Int -> Machine -> String
generateTikz time machine =
    let
        scale =
            40

        states =
            indtBy 4 <|
                List.map oneState <|
                    Dict.toList machine.statePositions

        stateName sId =
            case Dict.get sId machine.stateNames of
                Just n ->
                    n

                _ ->
                    ""

        statePos sId =
            case Dict.get sId machine.statePositions of
                Just p ->
                    p

                _ ->
                    ( 0, 0 )

        hashCode =
            String.dropRight 56 << sha256 << String.append (String.fromInt time)

        oneState ( sId, ( x, y ) ) =
            let
                ( tx, ty ) =
                    ( String.fromFloat <| x / scale, String.fromFloat <| y / scale )
            in
            String.concat [ "\\node[state,thick] at (", tx, ",", ty, ") (", hashCode <| stateName sId, ") {$", stateName sId, "$};" ]

        transitions =
            indtBy 4 <|
                List.map oneTransition <|
                    Dict.toList machine.stateTransitions

        oneTransition ( ( s0, tId, s1 ), ( x1, y1 ) ) =
            let
                transitionName =
                    case Dict.get tId machine.transitionNames of
                        Just n ->
                            renderSet2String n

                        _ ->
                            ""

                ( x0, y0 ) =
                    statePos s0

                ( x2, y2 ) =
                    statePos s1

                ( mx, my ) =
                    ( (x2 + x0) / 2 + rx, (y2 + y0) / 2 + ry )

                ( tx, ty ) =
                    --tangent between to and from states
                    ( x2 - x0, y2 - y0 )

                theta =
                    atan2 ty tx

                ( rx, ry ) =
                    ( x1 * cos theta - y1 * sin theta, y1 * cos theta + x1 * sin theta )

                inTheta =
                    round <| atan2 (my - y2) (mx - x2) * 180 / pi

                outTheta =
                    round <| atan2 (my - y0) (mx - x0) * 180 / pi

                position =
                    case labelPosition y1 theta of
                        Above ->
                            "above"

                        Below ->
                            "below"

                        Left ->
                            "left"

                        Right ->
                            "right"
            in
            String.concat [ "(", hashCode <| stateName s0, ") edge [", position, ",in = ", String.fromInt inTheta, ", out = ", String.fromInt outTheta, "] node {$", transitionName, "$} (", hashCode <| stateName s1, ")" ]
    in
    unlines
        [ "%% Machine generated by https://finsm.io"
        , "%% include in preamble:"
        , "%% \\usepackage{tikz}"
        , "%% \\usetikzlibrary{automata,positioning,arrows}"
        , "\\begin{tikzpicture}[]"
        , states
        , "    \\path[->, thick, >=stealth]"
        , transitions
        , "    ;"
        , "\\end{tikzpicture}"
        ]


unlines : List String -> String
unlines =
    String.concat << List.intersperse "\n"


indtBy : Int -> List String -> String
indtBy n =
    unlines << List.map ((++) (String.repeat n " "))
