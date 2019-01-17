module Simulating exposing (InputTape, Model(..), Msg(..), PersistentModel, delta, deltaHat, initPModel, isAccept, latexKeyboard, onEnter, onExit, renderTape, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Json.Decode as D
import Machine exposing (..)
import Set exposing (Set)
import SharedModel exposing (SharedModel)
import Task
import Tuple exposing (first, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown (D.map KeyPressed (D.field "keyCode" D.int))


type alias PersistentModel =
    { tapes : Dict Int (Array Character)
    , currentStates : Set StateID
    }


type alias InputTape =
    Array Character


type Model
    = Default Int {- tapeID -} Int {- charID -}
    | Editing Int


type Msg
    = Step
    | EditTape Int
    | DeleteTape Int
    | AddNewTape
    | ChangeTape Int
    | ToggleStart StateID
    | KeyPressed Int
    | MachineMsg Machine.Msg


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default 0 -1, pModel, sModel ), False, Cmd.none )


onExit : Environment -> ( Model, PersistentModel, SharedModel ) -> ( ( PersistentModel, SharedModel ), Bool )
onExit env ( model, pModel, sModel ) =
    ( ( pModel, sModel ), False )


initPModel : PersistentModel
initPModel =
    { tapes =
        Dict.fromList
            [ ( 0, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ] )
            , ( 1, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0" ] )
            ]
    , currentStates = test.start
    }


renderTape : Array String -> Int -> Int -> Int -> Bool -> Shape Msg
renderTape input tapeId selectedId inputAt showButtons =
    let
        xpad =
            20
    in
    group <|
        Array.toList
            (Array.indexedMap
                (\n st ->
                    group
                        [ square xpad
                            |> filled white
                            |> addOutline
                                (solid 1)
                                black
                            |> move ( 0, 3 )
                        , latex (xpad * 0.9) (xpad * 0.7) st AlignCentre
                            |> move ( 0, 10.25 )
                        ]
                        |> move ( toFloat n * xpad, 0 )
                        |> notifyTap (ChangeTape tapeId)
                )
                input
            )
            ++ (if tapeId == selectedId then
                    [ group
                        [ triangle 2.25
                            |> filled black
                            |> rotate (degrees 30)
                            |> move ( 0, xpad / 2 + 5.75 )
                        , triangle 2.25
                            |> filled black
                            |> rotate (degrees -30)
                            |> move ( 0, -xpad / 2 + 0.25 )
                        , rect 2 (xpad + 1)
                            |> filled black
                            |> move ( 0, 3 )
                        ]
                        |> move ( xpad / 2 + xpad * toFloat inputAt, 0 )
                    ]

                else
                    []
               )
            ++ (if showButtons then
                    [ group
                        [ roundedRect 15 15 2
                            |> filled white
                            |> addOutline (solid 1) darkGray
                        , editIcon
                            |> scale 1.5
                            |> move ( -3, -3 )
                            |> repaint black
                        ]
                        |> move ( toFloat <| Array.length input * xpad, 3 )
                        |> notifyTap (EditTape tapeId)
                    , group
                        [ roundedRect 15 15 2
                            |> filled white
                            |> addOutline (solid 1) darkGray
                        , trashIcon |> scale 0.2 |> move ( 0, -1 )
                        ]
                        |> move ( toFloat <| (Array.length input + 1) * xpad, 3 )
                        |> notifyTap (DeleteTape tapeId)
                    ]

                else
                    []
               )


update : Environment -> Msg -> ( Model, PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
update env msg ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine
    in
    case msg of
        Step ->
            case model of
                Default tapeId charId ->
                    let
                        nextCh =
                            case Dict.get tapeId pModel.tapes of
                                Just ar ->
                                    case Array.get (charId + 1) ar of
                                        Just ch ->
                                            ch

                                        _ ->
                                            ""

                                _ ->
                                    ""
                    in
                    if nextCh /= "" then
                        ( ( Default tapeId (charId + 1), { pModel | currentStates = deltaHat oldMachine.transitionNames oldMachine.delta nextCh pModel.currentStates }, sModel ), False, Cmd.none )

                    else
                        ( ( model, pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        EditTape tId ->
            ( ( Editing tId, pModel, sModel ), False, Cmd.none )

        DeleteTape tId ->
            let
                newModel =
                    case model of
                        Default tId0 chId ->
                            -- FIXME: choose a good tape to go to
                            if tId0 == tId then
                                Default 0 -1

                            else
                                Default tId0 chId

                        _ ->
                            model
            in
            ( ( newModel, { pModel | tapes = Dict.remove tId pModel.tapes }, sModel ), True, Cmd.none )

        AddNewTape ->
            let
                newId =
                    (case List.maximum <| Dict.keys pModel.tapes of
                        Just n ->
                            n

                        Nothing ->
                            0
                    )
                        + 1
            in
            ( ( model, { pModel | tapes = Dict.insert newId Array.empty pModel.tapes }, sModel ), True, Cmd.none )

        ChangeTape tId ->
            ( ( Default tId -1, { pModel | currentStates = oldMachine.start }, sModel ), False, Cmd.none )

        KeyPressed k ->
            if k == 13 then
                --pressed enter
                case model of
                    Editing tId ->
                        ( ( Default tId -1, pModel, sModel ), True, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == 8 then
                --pressed delete
                case model of
                    Editing tapeId ->
                        let
                            newPModel =
                                { pModel
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case m of
                                                    Just ar ->
                                                        Just <| Array.slice 0 -1 ar

                                                    _ ->
                                                        m
                                            )
                                            pModel.tapes
                                }
                        in
                        ( ( model, newPModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == 39 then
                --right arrow key
                case model of
                    Default _ _ ->
                        ( ( model, pModel, sModel ), False, Task.perform identity (Task.succeed <| Step) )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else
                case model of
                    Editing tapeId ->
                        let
                            charCode =
                                case k of
                                    65 ->
                                        0

                                    83 ->
                                        1

                                    68 ->
                                        2

                                    70 ->
                                        3

                                    71 ->
                                        4

                                    72 ->
                                        5

                                    74 ->
                                        6

                                    75 ->
                                        7

                                    76 ->
                                        8

                                    81 ->
                                        9

                                    87 ->
                                        10

                                    69 ->
                                        11

                                    82 ->
                                        12

                                    84 ->
                                        13

                                    89 ->
                                        14

                                    85 ->
                                        15

                                    73 ->
                                        16

                                    79 ->
                                        17

                                    80 ->
                                        18

                                    90 ->
                                        19

                                    88 ->
                                        20

                                    67 ->
                                        21

                                    86 ->
                                        22

                                    66 ->
                                        23

                                    78 ->
                                        24

                                    77 ->
                                        25

                                    _ ->
                                        -1

                            chars =
                                Array.fromList <| Set.toList <| Set.fromList <| Dict.values oldMachine.transitionNames

                            newChar =
                                Array.get charCode chars

                            newPModel =
                                { pModel
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case ( m, newChar ) of
                                                    ( Just ar, Just ch ) ->
                                                        Just <| Array.push ch ar

                                                    ( Nothing, Just ch ) ->
                                                        Just <| Array.fromList [ ch ]

                                                    _ ->
                                                        m
                                            )
                                            pModel.tapes
                                }
                        in
                        ( ( model, newPModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

        MachineMsg mmsg ->
            case mmsg of
                StartDragging sId _ ->
                    ( ( model, pModel, sModel ), False, Task.perform identity (Task.succeed <| ToggleStart sId) )

                SelectStateLabel sId ->
                    ( ( model, pModel, sModel ), False, Task.perform identity (Task.succeed <| ToggleStart sId) )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        ToggleStart sId ->
            let
                tests =
                    oldMachine.start

                newMachine =
                    { oldMachine
                        | start =
                            case Set.member sId oldMachine.start of
                                True ->
                                    Set.remove sId oldMachine.start

                                False ->
                                    Set.insert sId oldMachine.start
                    }
            in
            case model of
                Default tId _ ->
                    ( ( Default tId -1, { pModel | currentStates = newMachine.start }, { sModel | machine = newMachine } ), True, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )


isAccept : Set StateID -> Set StateID -> InputTape -> Int -> Bool
isAccept states finals input inputAt =
    if inputAt == Array.length input then
        Set.size (Set.intersect states finals) > 0

    else
        False


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine

        winX =
            toFloat <| first env.windowSize

        winY =
            toFloat <| second env.windowSize

        chars =
            Set.toList <| Set.fromList <| List.map (\( _, n ) -> n) <| Dict.toList oldMachine.transitionNames

        getStateName sId =
            case Dict.get sId oldMachine.stateNames of
                Just n ->
                    n

                Nothing ->
                    "\\ "

        menu =
            group
                [ text "Simulate"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 2, winY / 6 - 15 )
                , text "(Click to toggle start state(s), right arrow to scrub through tape)"
                    |> size 6
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 85, winY / 6 - 15 )
                , group
                    [ roundedRect 15 15 2
                        |> filled white
                        |> addOutline (solid 1) darkGray
                    , text "+"
                        |> size 16
                        |> fixedwidth
                        |> filled black
                        |> move ( -4.5, -5 )
                        |> notifyTap AddNewTape
                    ]
                    |> move ( -winX / 2 + 20, winY / 6 - 35 - 25 * (toFloat <| Dict.size pModel.tapes) )
                , text "Machine"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 492, winY / 6 - 15 )
                , latex 500 18 "let\\ N = (Q,\\Sigma,\\Delta,S,F)" AlignLeft
                    |> move ( -winX / 2 + 750, winY / 6 - 25 )
                , latex 500 14 "where" AlignLeft
                    |> move ( -winX / 2 + 750, winY / 6 - 45 )
                , latex 500 18 ("Q = \\{ " ++ String.join "," (Dict.values oldMachine.stateNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 65 )
                , latex 500 18 ("\\Sigma = \\{ " ++ String.join "," (Set.toList <| Set.fromList <| Dict.values oldMachine.transitionNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 90 )
                , latex 500 18 "\\Delta = (above)" AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 115 )
                , latex 500 18 ("S = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| oldMachine.start) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 140 )
                , latex 500 18 ("F = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| oldMachine.final) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 165 )
                , case model of
                    Default tapeId charId ->
                        group (List.indexedMap (\x ( chId, ch ) -> renderTape ch chId tapeId charId True |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList tapes)
                            |> move ( -winX / 2 + 20, winY / 6 - 40 )

                    _ ->
                        group []
                ]

        tapes =
            pModel.tapes
    in
    group
        [ case model of
            Default _ _ ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , menu
                    ]
                    |> move ( 0, -winY / 3 )

            Editing tapeId ->
                let
                    tape =
                        case Dict.get tapeId pModel.tapes of
                            Just t ->
                                t

                            Nothing ->
                                Array.empty
                in
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , text "Edit Tape"
                        |> size 16
                        |> fixedwidth
                        |> filled black
                        |> move ( -winX / 2 + 2, winY / 6 - 15 )
                    , text "(Type symbols with your keyboard; backspace to delete; enter to accept)"
                        |> size 6
                        |> fixedwidth
                        |> filled black
                        |> move ( -winX / 2 + 95, winY / 6 - 15 )
                    , latexKeyboard winX winY chars
                        |> move ( 0, 0 )
                    , renderTape tape tapeId -1 -1 False
                        |> move ( -10 * toFloat (Array.length tape), winY / 6 - 65 )
                    ]
                    |> move ( 0, -winY / 3 )
        , (GraphicSVG.map MachineMsg <| Machine.view env Regular sModel.machine pModel.currentStates) |> move ( 0, winY / 6 )
        ]


delta : TransitionNames -> Delta -> Character -> StateID -> Set StateID
delta tNames d ch state =
    let
        getName trans =
            case Dict.get trans tNames of
                Just n ->
                    n

                _ ->
                    ""
    in
    case Dict.get state d of
        Just transMap ->
            let
                states =
                    List.filterMap
                        (\( tId, sId ) ->
                            if getName tId == ch then
                                Just sId

                            else
                                Nothing
                        )
                    <|
                        Dict.toList transMap
            in
            Set.fromList states

        Nothing ->
            Set.empty


deltaHat : TransitionNames -> Delta -> Character -> Set StateID -> Set StateID
deltaHat tNames d ch states =
    Set.foldl (\curr ss -> Set.union ss (delta tNames d ch curr)) Set.empty states


latexKeyboard : Float -> Float -> List Character -> Shape Msg
latexKeyboard w h chars =
    let
        topRow =
            [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p' ]

        homeRow =
            [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]

        botRow =
            [ 'z', 'x', 'c', 'v', 'b', 'n', 'm' ]

        keyW =
            clamp 0 50 (min (w / 11) (keyH * 1.2))

        keyH =
            h / 18

        renderKey letter char =
            group
                [ roundedRect keyW keyH 2
                    |> filled white
                    |> addOutline (solid 0.5) black
                , text (String.fromChar letter)
                    |> fixedwidth
                    |> size 10
                    |> filled (rgb 150 150 150)
                    |> move ( -keyW / 2 + 2, keyH / 2 - 8 )
                , latex (keyW / 1.5) (keyH / 1.5) char AlignCentre
                    |> move ( 0, 10 )
                ]

        fillOutExtras n offset chs =
            let
                newL =
                    List.take n (List.drop offset chs)
            in
            newL ++ List.repeat (n - List.length newL) "\\ "

        oneRow letters chs =
            group
                (List.indexedMap
                    (\x ( c, l ) ->
                        renderKey l c
                            |> move ( (keyW + 2) * (toFloat x - (toFloat <| List.length chs) / 2) + keyW / 2 + w / 33, 0 )
                    )
                    (List.map2 (\a b -> ( a, b )) chs letters)
                )
    in
    group
        [ oneRow topRow (fillOutExtras 10 9 chars) |> move ( -keyW / 3, 0 )
        , oneRow homeRow (fillOutExtras 9 0 chars) |> move ( -keyW / 3, -keyH - 2 )
        , oneRow botRow (fillOutExtras 7 19 chars) |> move ( -keyW, -(keyH + 2) * 2 )
        ]
