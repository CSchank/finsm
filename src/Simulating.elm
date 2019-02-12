module Simulating exposing (InputTape, Model(..), Msg(..), PersistentModel, delta, deltaHat, initPModel, isAccept, latexKeyboard, machineCheck, onEnter, onExit, renderTape, subscriptions, update, view)

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


type MachineType
    = DFA
    | NFA


type Error
    = NoError
    | DFAError DFAErrorType StateID
    | EpsTransError


type DFAErrorType
    = HasEpsilon
    | Incomplete
    | Nondeterministic
    | Unsure -- Good for debugging?


type alias PersistentModel =
    { tapes : Dict Int (Array Character)
    , currentStates : Set StateID
    , machineType : MachineType
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
    | ChangeMachine MachineType
    | MachineMsg Machine.Msg


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default 0 -1, { pModel | currentStates = epsTrans sModel.machine.transitionNames sModel.machine.delta sModel.machine.start }, sModel ), False, Cmd.none )


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
    , machineType = DFA
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
                        , latex (xpad * 0.9) (xpad * 0.7) "white" st AlignCentre
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

        machineType =
            pModel.machineType
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
                        ( ( Default tapeId (charId + 1)
                          , { pModel
                                | currentStates =
                                    deltaHat oldMachine.transitionNames oldMachine.delta nextCh pModel.currentStates
                            }
                          , sModel
                          )
                        , False
                        , Cmd.none
                        )

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
            ( ( Default tId -1, { pModel | currentStates = epsTrans oldMachine.transitionNames oldMachine.delta oldMachine.start }, sModel ), False, Cmd.none )

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
                                Array.fromList <| Set.toList <| Set.remove "\\epsilon" <| List.foldr Set.union Set.empty <| Dict.values oldMachine.transitionNames

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

        ChangeMachine mtype ->
            case mtype of
                NFA ->
                    case pModel.machineType of
                        NFA ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                        DFA ->
                            case model of
                                Editing tId ->
                                    ( ( Default tId -1, { pModel | machineType = NFA }, sModel ), False, Cmd.none )

                                _ ->
                                    ( ( model, { pModel | machineType = NFA }, sModel ), False, Cmd.none )

                DFA ->
                    case pModel.machineType of
                        DFA ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                        NFA ->
                            let
                                startState =
                                    if Set.size oldMachine.start > 1 then
                                        Set.singleton <|
                                            (\x ->
                                                case x of
                                                    Just val ->
                                                        val

                                                    Nothing ->
                                                        -1
                                            )
                                            <|
                                                List.head <|
                                                    Set.toList oldMachine.start

                                    else
                                        oldMachine.start

                                newPModel =
                                    { pModel | machineType = DFA, currentStates = startState }

                                newSModel =
                                    { sModel | machine = { oldMachine | start = startState } }
                            in
                            case model of
                                Editing tId ->
                                    ( ( Default tId -1, newPModel, newSModel ), True, Cmd.none )

                                _ ->
                                    ( ( model, newPModel, newSModel ), True, Cmd.none )

        MachineMsg mmsg ->
            case mmsg of
                StartDragging sId _ ->
                    ( ( model, pModel, sModel ), False, sendMsg (ToggleStart sId) )

                TapState sId ->
                    ( ( model, pModel, sModel ), False, sendMsg (ToggleStart sId) )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        ToggleStart sId ->
            let
                tests =
                    oldMachine.start

                newMachine =
                    case machineType of
                        NFA ->
                            { oldMachine
                                | start =
                                    case Set.member sId oldMachine.start of
                                        True ->
                                            Set.remove sId oldMachine.start

                                        False ->
                                            Set.insert sId oldMachine.start
                            }

                        DFA ->
                            { oldMachine
                                | start = Set.singleton sId
                            }
            in
            case model of
                Default tId _ ->
                    ( ( Default tId -1, { pModel | currentStates = epsTrans oldMachine.transitionNames oldMachine.delta newMachine.start }, { sModel | machine = newMachine } ), True, Cmd.none )

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
            -- This is broken?
            Set.toList <| Set.remove "\\epsilon" <| List.foldr Set.union Set.empty <| Dict.values oldMachine.transitionNames

        menu =
            group <|
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
                , case model of
                    Default tapeId charId ->
                        group (List.indexedMap (\x ( chId, ch ) -> renderTape ch chId tapeId charId True |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList tapes)
                            |> move ( -winX / 2 + 20, winY / 6 - 40 )

                    _ ->
                        group []
                ]

        tapes =
            pModel.tapes

        validCheck =
            machineCheck sModel
    in
    group
        [ case model of
            Default _ _ ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , case pModel.machineType of
                        DFA ->
                            if validCheck == NoError then
                                menu

                            else
                                errorMenu validCheck oldMachine winX winY

                        NFA ->
                            if validCheck == EpsTransError then
                                errorMenu validCheck oldMachine winX winY

                            else
                                menu
                    , machineDefn sModel pModel.machineType winX winY
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
        , machineModeButtons pModel.machineType winX winY
        ]


errorMenu : Error -> Machine -> Float -> Float -> Shape Msg
errorMenu err mac winX winY =
    let
        errStId =
            case err of
                DFAError _ stId ->
                    case Dict.get stId mac.stateNames of
                        Just name ->
                            name

                        Nothing ->
                            ""

                _ ->
                    ""

        errorHeader txt =
            group
                [ triangle 20 |> filled red |> rotate 22.5
                , roundedRect 7.5 10 5 |> filled white |> move ( 0, 7.5 )
                , circle 3 |> filled white |> move ( 0, -2.5 )
                , text txt
                    |> size 20
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 20, 0 )
                ]
                |> scale 0.75
                |> move ( -winX / 2 + 20, winY / 6 - 20 )

        errorReason =
            group
                [ circle 3 |> filled red
                , (text <|
                    case err of
                        DFAError HasEpsilon _ ->
                            "Possible cause: There are epsilon transitions"

                        DFAError Incomplete _ ->
                            "Possible cause: There are missing transitions"

                        DFAError Nondeterministic _ ->
                            "Possible cause: There are extraneous transitions"

                        EpsTransError ->
                            "Cause: Epsilon transitions are mixed with normal transitions"

                        _ ->
                            "You might have missed something somewhere?"
                  )
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( -winX / 2 + 20, winY / 6 - 40 )

        errorHint =
            group
                [ circle 3 |> filled red
                , (text <|
                    case err of
                        DFAError HasEpsilon _ ->
                            "Hint: Try removing all your epsilon transitions"

                        DFAError Incomplete _ ->
                            "Hint: Check states for missing transitions"

                        DFAError Nondeterministic _ ->
                            "Hint: Find and remove extra transitions"

                        EpsTransError ->
                            "Hint: Switch to Build mode and fix transitions in red"

                        _ ->
                            ""
                  )
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( -winX / 2 + 20, winY / 6 - 60 )

        errorState =
            group
                [ circle 3 |> filled red
                , text "Hint: Check state "
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                , latex 50 12 "blank" errStId AlignLeft |> move ( 170, 3 )
                ]
                |> move ( -winX / 2 + 20, winY / 6 - 80 )

        actionHint =
            group
                [ circle 3 |> filled red
                , text "Go to Build mode to fix your machine, or use a NFA"
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( -winX / 2 + 20, winY / 6 - 100 )
    in
    case err of
        DFAError _ _ ->
            group [ errorHeader "DFA error: Your machine has a problem!", errorReason, errorHint, errorState, actionHint ]

        EpsTransError ->
            group [ errorHeader "Error: You have invalid state transitions!", errorReason, errorHint ]

        NoError ->
            group []


machineDefn : SharedModel -> MachineType -> Float -> Float -> Shape Msg
machineDefn sModel mtype winX winY =
    let
        machine =
            sModel.machine

        getStateName sId =
            case Dict.get sId machine.stateNames of
                Just n ->
                    n

                Nothing ->
                    "\\ "

        machineHeader =
            text "Machine"
                |> size 16
                |> fixedwidth
                |> filled black
                |> move ( -winX / 2 + 492, winY / 6 - 15 )
    in
    case mtype of
        NFA ->
            group
                [ machineHeader
                , latex 500 18 "blank" "let\\ N = (Q,\\Sigma,\\Delta,S,F)" AlignLeft
                    |> move ( -winX / 2 + 500, winY / 6 - 25 )
                , latex 500 14 "blank" "where" AlignLeft
                    |> move ( -winX / 2 + 500, winY / 6 - 45 )
                , latex 500 18 "blank" ("Q = \\{ " ++ String.join "," (Dict.values machine.stateNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 65 )
                , latex 500 18 "blank" ("\\Sigma = \\{ " ++ String.join "," (Set.toList <| Set.remove "\\epsilon" <| List.foldl Set.union Set.empty <| Dict.values machine.transitionNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 90 )
                , latex 500 18 "blank" "\\Delta = (above)" AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 115 )
                , latex 500 18 "blank" ("S = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| machine.start) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 140 )
                , latex 500 18 "blank" ("F = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| machine.final) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 165 )
                ]

        DFA ->
            group
                [ machineHeader
                , latex 500 18 "blank" "let\\ M = (Q,\\Sigma,\\delta,s,F)" AlignLeft
                    |> move ( -winX / 2 + 500, winY / 6 - 25 )
                , latex 500 14 "blank" "where" AlignLeft
                    |> move ( -winX / 2 + 500, winY / 6 - 45 )
                , latex 500 18 "blank" ("Q = \\{ " ++ String.join "," (Dict.values machine.stateNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 65 )
                , latex 500 18 "blank" ("\\Sigma = \\{ " ++ String.join "," (Set.toList <| Set.remove "\\epsilon" <| List.foldl Set.union Set.empty <| Dict.values machine.transitionNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 90 )
                , latex 500 18 "blank" "\\delta = (above)" AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 115 )
                , latex 500
                    14
                    "blank"
                    ("s = "
                        ++ (case Set.toList machine.start of
                                [] ->
                                    "Please\\ select\\ a\\ start\\ state"

                                x :: [] ->
                                    getStateName x

                                x :: xs ->
                                    "Congratulations,\\ you\\ found\\ a\\ bug!"
                           )
                    )
                    AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 140 )
                , latex 500 18 "blank" ("F = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| machine.final) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 510, winY / 6 - 160 )
                ]


epsTrans : TransitionNames -> Delta -> Set StateID -> Set StateID
epsTrans tNames d states =
    let
        dList =
            (Dict.toList << Dict.filter (\k _ -> Set.member k states)) d

        -- LMD: This was copy-pasted from delta
        getName trans =
            case Dict.get trans tNames of
                Just n ->
                    renderSet2String n

                _ ->
                    ""

        findEpsTransitions : List ( StateID, Dict TransitionID StateID ) -> List StateID
        findEpsTransitions lst =
            case lst of
                [] ->
                    []

                ( sID, dictTrans ) :: xs ->
                    let
                        listTrans =
                            Dict.toList dictTrans

                        epsStates =
                            List.filterMap
                                (\( tId, sId ) ->
                                    if getName tId == "\\epsilon" then
                                        Just sId

                                    else
                                        Nothing
                                )
                                listTrans
                    in
                    epsStates ++ findEpsTransitions xs

        newCurrentStates =
            Set.union (Set.fromList <| findEpsTransitions dList) states
    in
    if newCurrentStates == states then
        states

    else
        epsTrans tNames d newCurrentStates


delta : TransitionNames -> Delta -> Character -> StateID -> Set StateID
delta tNames d ch state =
    let
        getName trans =
            case Dict.get trans tNames of
                Just n ->
                    n

                _ ->
                    Set.empty
    in
    case Dict.get state d of
        Just transMap ->
            let
                states =
                    List.filterMap
                        (\( tId, sId ) ->
                            if
                                (Set.member ch <| getName tId)
                                    || ((renderSet2String <| getName tId) == "\\epsilon" && sId == state)
                            then
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
    let
        newStates =
            Set.foldl (\curr ss -> Set.union ss (delta tNames d ch curr)) Set.empty states
    in
    epsTrans tNames d newStates


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
                , latex (keyW / 1.5) (keyH / 1.5) "white" char AlignCentre
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


machineModeButtons : MachineType -> Float -> Float -> Shape Msg
machineModeButtons mtype winX winY =
    group
        [ group
            [ roundedRect 30 15 1
                |> filled
                    (if mtype == DFA then
                        finsmLightBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "DFA"
                |> centered
                |> fixedwidth
                |> filled
                    (if mtype == DFA then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 20, winY / 2 - 32 )
            |> notifyTap (ChangeMachine DFA)
        , group
            [ roundedRect 30 15 1
                |> filled
                    (if mtype == NFA then
                        finsmLightBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "NFA"
                |> centered
                |> fixedwidth
                |> filled
                    (if mtype == NFA then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 52, winY / 2 - 32 )
            |> notifyTap (ChangeMachine NFA)
        ]


machineCheck : SharedModel -> Error
machineCheck sModel =
    let
        mac =
            sModel.machine

        tMistakes =
            sModel.machine.transitionMistakes

        allTransitionLabels =
            List.sort <| Set.toList <| Set.remove "\\epsilon" <| List.foldr Set.union Set.empty <| Dict.values mac.transitionNames

        catch : Maybe (Set String) -> List String
        catch ms =
            case ms of
                Nothing ->
                    []

                Just s ->
                    Set.toList s

        getTrans : Dict TransitionID StateID -> List String
        getTrans d =
            (List.concatMap (\e -> Dict.get e mac.transitionNames |> catch) <| Dict.keys d) |> List.sort

        foldingFunc : ( StateID, Dict TransitionID StateID ) -> Error -> Error
        foldingFunc sTuple err =
            case err of
                DFAError errType x ->
                    DFAError errType x

                NoError ->
                    let
                        transitions =
                            getTrans <| second sTuple

                        stId =
                            first sTuple
                    in
                    if transitions == allTransitionLabels then
                        NoError

                    else if List.member "\\epsilon" transitions then
                        DFAError HasEpsilon stId

                    else
                        case compare (List.length transitions) (List.length allTransitionLabels) of
                            LT ->
                                DFAError Incomplete stId

                            EQ ->
                                DFAError Incomplete stId

                            -- e.g. compare [1,1,2] [1,2,3], can be Nondeterministic too
                            GT ->
                                DFAError Nondeterministic stId

                EpsTransError ->
                    EpsTransError
    in
    if tMistakes /= Nothing then
        EpsTransError

    else
        List.foldr (\x acc -> foldingFunc x acc) NoError <| Dict.toList mac.delta
