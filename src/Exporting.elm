module Exporting exposing (InputTape, Model(..), Msg(..), Output(..), PersistentModel, exportButton, exportTikz, generateTikz, indtBy, initPModel, onEnter, onExit, output, subscriptions, unlines, update, view)

import Array exposing (Array)
import Browser.Events
import Dict exposing (Dict)
import Environment exposing (Environment)
import Error exposing (..)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Machine exposing (..)
import Mistakes exposing (..)
import Set exposing (Set)
import Sha256 exposing (sha256)
import SharedModel exposing (..)
import Task
import Time exposing (Month(..), customZone, millisToPosix, toDay, toHour, toMinute, toMonth, toSecond, toYear)
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
    | HoverError
    | ShowingOutput


type Output
    = Tikz


type Msg
    = SelectOutput Output
    | GenerateOutput
    | CloseOutput
    | MachineMsg Machine.Msg
    | GetTime Int
    | HoverErrorEnter
    | HoverErrorExit


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default, pModel, sModel ), False, Cmd.none )


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
            ( ( ShowingOutput, pModel, sModel ), False, Task.perform (GetTime << Time.posixToMillis) Time.now )

        CloseOutput ->
            ( ( Default, pModel, sModel ), False, Cmd.none )

        MachineMsg mmsg ->
            ( ( model, pModel, sModel ), False, Cmd.none )

        GetTime t ->
            ( ( model, { pModel | time = t }, sModel ), False, Cmd.none )

        HoverErrorEnter ->
            ( ( HoverError, pModel, sModel ), False, Cmd.none )

        HoverErrorExit ->
            ( ( Default, pModel, sModel ), False, Cmd.none )


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

        errCheck =
            machineCheck sModel

        hasErr =
            contextHasError errCheck sModel.machineType

        transMistakes =
            getTransitionMistakes oldMachine

        -- TODO: Adjust popup box size to fix custom error messages
        errHover =
            group
                [ errorIcon red white
                , if model == HoverError then
                    group [ roundedRect 465 110 5 |> filled darkGrey |> move ( 215, -55 ), errorMenu errCheck sModel.machine winX winY ]

                  else
                    group []
                ]
                |> notifyEnter HoverErrorEnter
                |> notifyLeave HoverErrorExit
                |> move ( winX / 6 - 100, -105 )
    in
    group
        [ (GraphicSVG.map MachineMsg <| Machine.view env Regular sModel.machineType sModel.machine Set.empty transMistakes) |> move ( -winX / 6, 0 )
        , machineSelected sModel.machineType winX winY
        , text "Choose format:"
            |> size 20
            |> fixedwidth
            |> filled black
            |> move ( winX / 6 - 125, 80 )
        , exportTikz (pModel.outputType == Tikz) |> move ( winX / 6, 0 )
        , exportButton (not hasErr)
            |> move ( winX / 6, -100 )
            |> (if hasErr then
                    identity

                else
                    notifyTap GenerateOutput
               )
        , if hasErr then
            errHover

          else
            group []
        , case ( model, pModel.outputType ) of
            ( ShowingOutput, Tikz ) ->
                output (winX / 2) (winY / 2) (generateTikz pModel.time sModel.machine)

            _ ->
                group []
        ]


machineSelected : MachineType -> Float -> Float -> Shape Msg
machineSelected mtype winX winY =
    let
        mtypeStr =
            case mtype of
                DFA ->
                    "DFA"

                NFA ->
                    "NFA"
    in
    text ("Your exported machine type: " ++ mtypeStr)
        |> centered
        |> fixedwidth
        |> filled darkGray
        |> move ( -winX / 2 + 117, winY / 2 - 32 )


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


exportButton clickable =
    group
        [ roundedRect 130 40 5
            |> filled
                (if clickable then
                    finsmBlue

                 else
                    gray
                )
        , text "Export"
            |> fixedwidth
            |> size 24
            |> centered
            |> filled
                (if clickable then
                    white

                 else
                    darkGray
                )
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

        dateStr =
            timeToString time

        hashCode =
            String.dropRight 56 << sha256 << String.append dateStr

        oneState ( sId, ( x, y ) ) =
            let
                ( tx, ty ) =
                    ( String.fromFloat <| x / scale, String.fromFloat <| y / scale )

                start =
                    if Set.member sId machine.start then
                        "initial,thick,"

                    else
                        "thick,"

                --"initial,thick," else "thick," --
                final =
                    if Set.member sId machine.final then
                        "accepting,"

                    else
                        ""
            in
            String.concat [ "\\node[", start, final, "state] at (", tx, ",", ty, ") (", hashCode <| stateName sId, ") {$", stateName sId, "$};" ]

        transitions =
            indtBy 4 <|
                List.map oneTransition <|
                    Dict.toList machine.stateTransitions

        oneTransition ( ( s0, tId, s1 ), ( x1, y1 ) ) =
            let
                transitionName =
                    case Dict.get tId machine.transitionNames of
                        Just n ->
                            renderSet2String n.inputLabel

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

                r =
                    20

                -- radius of states
                theta =
                    atan2 ty tx

                ( rx, ry ) =
                    ( x1 * cos theta - y1 * sin theta, y1 * cos theta + x1 * sin theta )

                ( inTheta, outTheta ) =
                    if s0 == s1 then
                        let
                            mr =
                                sqrt ((mx - x0) ^ 2 + (my - y0) ^ 2)

                            mpl =
                                mr - r

                            beta =
                                atan2 ry rx

                            gamma =
                                atan2 mpl mr
                        in
                        ( round <| (beta + gamma) * 180 / pi, round <| (beta - gamma) * 180 / pi )

                    else
                        ( round <| atan2 (my - y2) (mx - x2) * 180 / pi
                        , round <| atan2 (my - y0) (mx - x0) * 180 / pi
                        )

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

                loop =
                    if s0 == s1 then
                        let
                            loopDistance =
                                String.fromFloat <| roundPrec 2 <| sqrt (x1 ^ 2 + y1 ^ 2) / 40
                        in
                        String.concat [ "loop,min distance = ", loopDistance, "cm," ]

                    else
                        ""
            in
            String.concat [ "(", hashCode <| stateName s0, ") edge [", loop, position, ",in = ", String.fromInt inTheta, ", out = ", String.fromInt outTheta, "] node {$", transitionName, "$} (", hashCode <| stateName s1, ")" ]
    in
    unlines
        [ "%% Machine generated by https://finsm.io"
        , String.concat [ "%% ", dateStr ]
        , "%% include in preamble:"
        , "%% \\usepackage{tikz}"
        , "%% \\usetikzlibrary{automata,positioning,arrows}"
        , "\\begin{center}"
        , "\\begin{tikzpicture}[]"
        , states
        , "    \\path[->, thick, >=stealth]"
        , transitions
        , "    ;"
        , "\\end{tikzpicture}"
        , "\\end{center}"
        ]


est =
    customZone (-5 * 60) []


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


timeToString : Int -> String
timeToString timestamp =
    let
        year =
            toYear est (millisToPosix timestamp)

        month =
            toMonth est (millisToPosix timestamp)

        day =
            toDay est (millisToPosix timestamp)

        hour =
            toHour est (millisToPosix timestamp)

        minute =
            toMinute est (millisToPosix timestamp)

        second =
            toSecond est (millisToPosix timestamp)
    in
    String.fromInt year
        ++ "-"
        ++ String.fromInt (monthToInt month)
        ++ "-"
        ++ String.fromInt day
        ++ "-"
        ++ String.fromInt hour
        ++ ":"
        ++ (if minute < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt minute
        ++ ":"
        ++ (if minute < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt second


unlines : List String -> String
unlines =
    String.concat << List.intersperse "\n"


indtBy : Int -> List String -> String
indtBy n =
    unlines << List.map ((++) (String.repeat n " "))
