module Simulating exposing (HoverError, InputTape, Model(..), Msg(..), PersistentModel, TapeStatus(..), checkTape, checkTapes, delta, deltaHat, epsTrans, initPModel, isAccept, latexKeyboard, machineDefn, machineModeButtons, onEnter, onExit, renderTape, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events
import Debug
import Dict exposing (Dict)
import Environment exposing (Environment)
import Error exposing (..)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Json.Decode as D
import Machine exposing (..)
import Set exposing (Set)
import SharedModel exposing (..)
import Task
import Tuple exposing (first, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown (D.map KeyPressed (D.field "key" D.string))


type alias PersistentModel =
    { tapes : Dict Int ( InputTape, TapeStatus )
    , currentStates : Set StateID
    }


type alias InputTape =
    Array Character


type TapeStatus
    = Fresh
    | Stale (Set String)


type alias HoverError =
    Maybe Int


type Model
    = Default Int {- tapeID -} Int {- charID -} HoverError
    | Editing Int


type Msg
    = Step
    | EditTape Int
    | DeleteTape Int
    | AddNewTape
    | ChangeTape Int
    | ToggleStart StateID
    | KeyPressed String
    | ChangeMachine MachineType
    | MachineMsg Machine.Msg
    | HoverErrorEnter Int
    | HoverErrorExit


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default 0 -1 Nothing
      , { pModel
            | currentStates =
                epsTrans
                    sModel.machine.transitionNames
                    sModel.machine.delta
                    sModel.machine.start
            , tapes = checkTapes sModel pModel.tapes
        }
      , sModel
      )
    , False
    , Cmd.none
    )


onExit : Environment -> ( Model, PersistentModel, SharedModel ) -> ( ( PersistentModel, SharedModel ), Bool )
onExit env ( model, pModel, sModel ) =
    ( ( pModel, sModel ), False )


initPModel : PersistentModel
initPModel =
    { tapes =
        Dict.fromList
            [ ( 0, ( Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ], Fresh ) )
            , ( 1, ( Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0" ], Fresh ) )
            ]
    , currentStates = test.start
    }


checkTapes : SharedModel -> Dict Int ( InputTape, TapeStatus ) -> Dict Int ( InputTape, TapeStatus )
checkTapes sModel tapes =
    Dict.map (\k ( tape, _ ) -> ( tape, checkTape sModel tape )) tapes


checkTape : SharedModel -> InputTape -> TapeStatus
checkTape sModel inp =
    let
        tNames =
            sModel.machine.transitionNames

        allTransitionLabels =
            List.foldr Set.union Set.empty <| Dict.values tNames

        arrFilter =
            Array.filter (\v -> not <| Set.member v allTransitionLabels) inp
    in
    case Array.isEmpty arrFilter of
        True ->
            Fresh

        False ->
            Stale <| Set.fromList <| Array.toList arrFilter


renderTape : Model -> Array String -> TapeStatus -> Int -> Int -> Int -> Bool -> Shape Msg
renderTape model input tapeSt tapeId selectedId inputAt showButtons =
    let
        hoverOn =
            case model of
                Default _ _ (Just errId) ->
                    if errId == tapeId then
                        True

                    else
                        False

                _ ->
                    False

        xpad =
            20

        errWindow =
            group
                [ roundedRect 800 30 2
                    |> filled white
                    |> addOutline (solid 1) darkGray
                    |> move ( 400, 5 )
                , text "This tape has stale transitions. Modify or delete it!"
                    |> size 25
                    |> fixedwidth
                    |> filled red
                ]
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
                                (if tapeSt == Fresh then
                                    black

                                 else
                                    red
                                )
                            |> move ( 0, 3 )
                        , latex (xpad * 0.9) (xpad * 0.7) "white" st AlignCentre
                            |> move ( 0, 10.25 )
                        ]
                        |> move
                            ( toFloat n * xpad
                                + (if not showButtons then
                                    xpad / 2

                                   else
                                    0
                                  )
                            , 0
                            )
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
                    , if not (tapeSt == Fresh) then
                        group
                            ([ triangle 20 |> filled red |> rotate 22.5
                             , roundedRect 7.5 10 5 |> filled white |> move ( 0, 7.5 )
                             , circle 3 |> filled white |> move ( 0, -2.5 )
                             ]
                                ++ (if hoverOn then
                                        [ errWindow ]

                                    else
                                        []
                                   )
                            )
                            |> scale 0.5
                            |> move ( toFloat <| (Array.length input + 2) * xpad, 1 )
                            |> notifyEnter (HoverErrorEnter tapeId)
                            |> notifyLeave HoverErrorExit

                      else
                        group []
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
            sModel.machineType
    in
    case msg of
        Step ->
            case model of
                Default tapeId charId hover ->
                    let
                        nextCh =
                            case Dict.get tapeId pModel.tapes of
                                Just ( ar, tapeStatus ) ->
                                    case Array.get (charId + 1) ar of
                                        Just ch ->
                                            if tapeStatus == Fresh then
                                                ch

                                            else
                                                ""

                                        _ ->
                                            ""

                                _ ->
                                    ""
                    in
                    if nextCh /= "" then
                        ( ( Default tapeId (charId + 1) hover
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
                        Default tId0 chId hover ->
                            -- FIXME: choose a good tape to go to
                            if tId0 == tId then
                                Default 0 -1 hover

                            else
                                Default tId0 chId hover

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
            ( ( model, { pModel | tapes = Dict.insert newId ( Array.empty, Fresh ) pModel.tapes }, sModel ), True, Cmd.none )

        ChangeTape tId ->
            ( ( Default tId -1 Nothing {- ??? -}, { pModel | currentStates = epsTrans oldMachine.transitionNames oldMachine.delta oldMachine.start }, sModel ), False, Cmd.none )

        KeyPressed k ->
            if k == "Enter" then
                case model of
                    Editing tId ->
                        ( ( Default tId -1 Nothing, { pModel | currentStates = epsTrans oldMachine.transitionNames oldMachine.delta oldMachine.start }, sModel ), True, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "Backspace" || k == "ArrowLeft" then
                case model of
                    Editing tapeId ->
                        let
                            newPModel =
                                { pModel
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case m of
                                                    Just ( ar, tapeSt ) ->
                                                        let
                                                            newTape =
                                                                Array.slice 0 -1 ar

                                                            freshSt =
                                                                checkTape sModel newTape
                                                        in
                                                        Just ( Array.slice 0 -1 ar, freshSt )

                                                    _ ->
                                                        m
                                            )
                                            pModel.tapes
                                }
                        in
                        ( ( model, newPModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "ArrowRight" then
                case model of
                    Default _ _ _ ->
                        ( ( model, pModel, sModel ), False, Task.perform identity (Task.succeed <| Step) )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "ArrowLeft" then
                case model of
                    Default tId _ hErr ->
                        ( ( Default tId -1 hErr, { pModel | currentStates = sModel.machine.start }, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else
                case model of
                    Editing tapeId ->
                        let
                            charCode =
                                case k of
                                    "a" ->
                                        0

                                    "s" ->
                                        1

                                    "d" ->
                                        2

                                    "f" ->
                                        3

                                    "g" ->
                                        4

                                    "h" ->
                                        5

                                    "j" ->
                                        6

                                    "k" ->
                                        7

                                    "l" ->
                                        8

                                    "q" ->
                                        9

                                    "w" ->
                                        10

                                    "e" ->
                                        11

                                    "r" ->
                                        12

                                    "t" ->
                                        13

                                    "y" ->
                                        14

                                    "u" ->
                                        15

                                    "i" ->
                                        16

                                    "o" ->
                                        17

                                    "p" ->
                                        18

                                    "z" ->
                                        19

                                    "x" ->
                                        20

                                    "c" ->
                                        21

                                    "v" ->
                                        22

                                    "b" ->
                                        23

                                    "n" ->
                                        24

                                    "m" ->
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
                                                    ( Just ( ar, tapeSt ), Just ch ) ->
                                                        Just ( Array.push ch ar, tapeSt )

                                                    ( Nothing, Just ch ) ->
                                                        Just ( Array.fromList [ ch ], Fresh )

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
                    case sModel.machineType of
                        NFA ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                        DFA ->
                            case model of
                                Editing tId ->
                                    ( ( Default tId -1 Nothing, pModel, { sModel | machineType = NFA } ), False, Cmd.none )

                                _ ->
                                    ( ( model, pModel, { sModel | machineType = NFA } ), False, Cmd.none )

                DFA ->
                    case sModel.machineType of
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
                                    { pModel | currentStates = startState }

                                newSModel =
                                    { sModel | machine = { oldMachine | start = startState }, machineType = DFA }
                            in
                            case model of
                                Editing tId ->
                                    ( ( Default tId -1 Nothing, newPModel, newSModel ), True, Cmd.none )

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
                Default tId _ _ ->
                    ( ( Default tId -1 Nothing, { pModel | currentStates = epsTrans oldMachine.transitionNames oldMachine.delta newMachine.start }, { sModel | machine = newMachine } ), True, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        HoverErrorEnter tapeId ->
            case model of
                Default tId pos _ ->
                    ( ( Default tId pos (Just tapeId), pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        HoverErrorExit ->
            case model of
                Default tId pos _ ->
                    ( ( Default tId pos Nothing, pModel, sModel ), False, Cmd.none )

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
                    Default tapeId charId _ ->
                        group (List.indexedMap (\x ( chId, ( ch, tapeSt ) ) -> renderTape model ch tapeSt chId tapeId charId True |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList tapes)
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
            Default _ _ _ ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , machineDefn sModel sModel.machineType winX winY
                    , if contextHasError validCheck sModel.machineType then
                        errorMenu validCheck oldMachine winX winY |> move ( -winX / 2 + 20, winY / 6 )

                      else
                        menu
                    ]
                    |> move ( 0, -winY / 3 )

            Editing tapeId ->
                let
                    ( tape, tapeSt ) =
                        case Dict.get tapeId pModel.tapes of
                            Just ( t, st ) ->
                                ( t, st )

                            Nothing ->
                                ( Array.empty, Fresh )
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
                    , renderTape model tape tapeSt tapeId -1 -1 False
                        |> move ( -10 * toFloat (Array.length tape), winY / 6 - 65 )
                    ]
                    |> move ( 0, -winY / 3 )
        , (GraphicSVG.map MachineMsg <| Machine.view env Regular sModel.machine pModel.currentStates) |> move ( 0, winY / 6 )
        , machineModeButtons sModel.machineType winX winY
        ]


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
