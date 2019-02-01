module Building exposing (Model, Msg(..), PersistentModel(..), editingButtons, icon, init, initPModel, onEnter, onExit, subscriptions, update, updateArrowPos, updateStatePos, view)

import Browser.Events
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Json.Decode as D
import Machine exposing (..)
import Set
import SharedModel exposing (SharedModel)
import Task
import Tuple exposing (first, second)


type alias Model =
    { machineState : Machine.Model
    }


type PersistentModel
    = Empty


type Msg
    = MachineMsg Machine.Msg
    | SaveStateName StateID String
    | SaveTransitionName TransitionID String
    | AddState ( Float, Float )
    | KeyPressed Int
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map KeyPressed (D.field "keyCode" D.int))
        ]


init : Model
init =
    { machineState = Regular
    }


initPModel : PersistentModel
initPModel =
    Empty


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( init, pModel, sModel ), False, Cmd.none )


onExit : Environment -> ( Model, PersistentModel, SharedModel ) -> ( ( PersistentModel, SharedModel ), Bool )
onExit env ( model, pModel, sModel ) =
    ( ( pModel, sModel ), False )


update : Environment -> Msg -> ( Model, PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
update env msg ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine
    in
    case msg of
        MachineMsg mmsg ->
            case mmsg of
                StartDragging st ( x, y ) ->
                    let
                        ( sx, sy ) =
                            case Dict.get st oldMachine.statePositions of
                                Just ( xx, yy ) ->
                                    ( xx, yy )

                                Nothing ->
                                    ( 0, 0 )
                    in
                    case model.machineState of
                        MousingOverRim sId _ ->
                            ( ( { model | machineState = AddingArrow sId ( x, y ) }, pModel, sModel ), False, Cmd.none )

                        _ ->
                            ( ( { model | machineState = DraggingState st ( x - sx, y - sy ) }, pModel, sModel ), False, Cmd.none )

                StartDraggingArrow ( st1, char, st2 ) ->
                    ( ( { model | machineState = DraggingArrow ( st1, char, st2 ) }, pModel, sModel ), False, Cmd.none )

                StartMouseOverRim stId ( x, y ) ->
                    case model.machineState of
                        Regular ->
                            ( ( { model | machineState = MousingOverRim stId ( x, y ) }, pModel, sModel ), False, Cmd.none )

                        _ ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                MoveMouseOverRim ( x, y ) ->
                    case model.machineState of
                        MousingOverRim stId _ ->
                            ( ( { model | machineState = MousingOverRim stId ( x, y ) }, pModel, sModel ), False, Cmd.none )

                        _ ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                StopMouseOverRim ->
                    case model.machineState of
                        MousingOverRim _ _ ->
                            ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )
                        _ -> ( ( model, pModel, sModel ), False, Cmd.none )

                StopDragging ->
                    case model.machineState of
                        DraggingState st _ ->
                            ( ( { model | machineState = SelectedState st }, pModel, sModel ), True, Cmd.none )

                        AddingArrowOverOtherState st _ s1 ->
                            let
                                newTrans =
                                    case List.head <| Dict.values oldMachine.transitionNames of
                                        Just char ->
                                            renderSet2String char

                                        Nothing ->
                                            "x"

                                newTransID =
                                    case List.maximum <| Dict.keys oldMachine.transitionNames of
                                        Just n ->
                                            n + 1

                                        Nothing ->
                                            0

                                newDelta : Delta
                                newDelta =
                                    Dict.update st
                                        (\mcDict ->
                                            case mcDict of
                                                Just ss ->
                                                    Just <|
                                                        Dict.update newTransID
                                                            (\mState ->
                                                                Just s1
                                                            )
                                                            ss

                                                Nothing ->
                                                    Just <| Dict.singleton newTransID s1
                                        )
                                        oldMachine.delta
                            in
                            ( ( { model | machineState = Regular }
                              , pModel
                              , { sModel
                                    | machine =
                                        { oldMachine
                                            | delta = newDelta
                                            , transitionNames = Dict.insert newTransID (parseString2Set newTrans) oldMachine.transitionNames
                                            , stateTransitions = Dict.insert ( st, newTransID, s1 ) ( 0, 0 ) oldMachine.stateTransitions
                                        }
                                }
                              )
                            , True
                            , Cmd.none
                            )

                        DraggingArrow tId ->
                            ( ( { model | machineState = Regular }, pModel, sModel ), True, Cmd.none )

                        _ ->
                            ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

                SelectArrow ( s0, char, s1 ) ->
                    ( ( { model | machineState = SelectedArrow ( s0, char, s1 ) }, pModel, sModel ), False, Cmd.none )

                Drag ( x, y ) ->
                    case model.machineState of
                        DraggingState st ( ox, oy ) ->
                            let
                                ( sx, sy ) =
                                    case Dict.get st oldMachine.statePositions of
                                        Just ( xx, yy ) ->
                                            ( xx, yy )

                                        Nothing ->
                                            ( 0, 0 )
                            in
                            ( ( { model | machineState = model.machineState }, pModel, { sModel | machine = { oldMachine | statePositions = updateStatePos st ( x - ox, y - oy ) oldMachine.statePositions } } )
                            , False
                            , Cmd.none
                            )

                        DraggingArrow ( s1, char, s2 ) ->
                            let
                                ( x0, y0 ) =
                                    case Dict.get s1 oldMachine.statePositions of
                                        Just ( xx, yy ) ->
                                            ( xx, yy )

                                        Nothing ->
                                            ( 0, 0 )

                                ( x1, y1 ) =
                                    case Dict.get s2 oldMachine.statePositions of
                                        Just ( xx, yy ) ->
                                            ( xx, yy )

                                        Nothing ->
                                            ( 0, 0 )

                                theta =
                                    -1 * atan2 (y1 - y0) (x1 - x0)

                                ( mx, my ) =
                                    ( (x0 + x1) / 2, (y0 + y1) / 2 )

                                ( nx, ny ) =
                                    sub ( x, y ) ( mx, my )

                                nprot =
                                    ( nx * cos theta - ny * sin theta, nx * sin theta + ny * cos theta )
                            in
                            ( ( { model | machineState = model.machineState }, pModel, { sModel | machine = { oldMachine | stateTransitions = Dict.insert ( s1, char, s2 ) nprot oldMachine.stateTransitions } } ), False, Cmd.none )

                        AddingArrow st _ ->
                            let
                                aboveStates =
                                    List.map (\( sId, _ ) -> sId) <|
                                        Dict.toList <|
                                            Dict.filter (\_ ( x1, y1 ) -> (x1 - x) ^ 2 + (y1 - y) ^ 2 <= 400) oldMachine.statePositions

                                newState =
                                    case aboveStates of
                                        h :: _ ->
                                            AddingArrowOverOtherState st ( x, y ) h

                                        _ ->
                                            AddingArrow st ( x, y )
                            in
                            ( ( { model | machineState = newState }, pModel, sModel ), False, Cmd.none )

                        AddingArrowOverOtherState st _ s1 ->
                            let
                                aboveStates =
                                    List.map (\( sId, _ ) -> sId) <|
                                        Dict.toList <|
                                            Dict.filter (\_ ( x1, y1 ) -> (x1 - x) ^ 2 + (y1 - y) ^ 2 <= 400) oldMachine.statePositions

                                newState =
                                    case aboveStates of
                                        h :: _ ->
                                            AddingArrowOverOtherState st ( x, y ) h

                                        _ ->
                                            AddingArrow st ( x, y )
                            in
                            ( ( { model | machineState = newState }, pModel, sModel ), False, Cmd.none )

                        _ ->
                            ( ( { model | machineState = model.machineState }, pModel, sModel ), False, Cmd.none )

                MouseOverStateLabel st ->
                    ( ( { model | machineState = MousingOverStateLabel st }, pModel, sModel ), False, Cmd.none )

                MouseOverTransitionLabel tr ->
                    let
                        newState =
                            case model.machineState of
                                Regular ->
                                    MousingOverTransitionLabel tr

                                _ ->
                                    model.machineState
                    in
                    ( ( { model | machineState = newState }, pModel, sModel ), False, Cmd.none )

                MouseLeaveLabel ->
                    let
                        newState =
                            case model.machineState of
                                MousingOverStateLabel _ ->
                                    Regular

                                MousingOverTransitionLabel _ ->
                                    Regular

                                _ ->
                                    model.machineState
                    in
                    ( ( { model | machineState = newState }, pModel, sModel ), False, Cmd.none )

                SelectStateLabel st ->
                    let
                        stateName =
                            case Dict.get st oldMachine.stateNames of
                                Just n ->
                                    n

                                Nothing ->
                                    ""
                    in
                    ( ( { model | machineState = EditingStateLabel st stateName }, pModel, sModel ), False, focusInput NoOp )

                SelectTransitionLabel tr ->
                    let
                        transName =
                            case Dict.get tr oldMachine.transitionNames of
                                Just n ->
                                    renderSet2String n

                                Nothing ->
                                    ""
                    in
                    ( ( { model | machineState = EditingTransitionLabel tr transName }, pModel, sModel ), False, focusInput NoOp )

                EditLabel _ lbl ->
                    let
                        newState =
                            case model.machineState of
                                EditingStateLabel st _ ->
                                    EditingStateLabel st lbl

                                EditingTransitionLabel tr _ ->
                                    EditingTransitionLabel tr lbl

                                _ ->
                                    model.machineState
                    in
                    ( ( { model | machineState = newState }, pModel, sModel ), False, Cmd.none )

                TapState sId ->
                    ( ( { model | machineState = SelectedState sId }, pModel, sModel ), False, Cmd.none )

                Reset ->
                    ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

        AddState ( x, y ) ->
            case model.machineState of
                Regular ->
                    let
                        newId =
                            setMax oldMachine.q + 1

                        newMachine =
                            { oldMachine
                                | q = Set.insert newId oldMachine.q
                                , statePositions = Dict.insert newId ( x, y ) oldMachine.statePositions
                                , stateNames = Dict.insert newId ("q_{" ++ String.fromInt newId ++ "}") oldMachine.stateNames
                            }
                    in
                    ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                _ ->
                    ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

        KeyPressed k ->
            if k == 13 then
                --pressed enter
                case model.machineState of
                    EditingStateLabel sId newLbl ->
                        let
                            oldStateName =
                                case Dict.get sId oldMachine.stateNames of
                                    Just n ->
                                        n

                                    _ ->
                                        ""
                        in
                        if newLbl == oldStateName || newLbl == "" then
                            ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

                        else
                            ( ( { model | machineState = Regular }, pModel, sModel ), True, sendMsg <| SaveStateName sId newLbl )

                    EditingTransitionLabel tId newLbl ->
                        let
                            oldTransitionName =
                                case Dict.get tId oldMachine.transitionNames of
                                    Just n ->
                                        renderSet2String n

                                    _ ->
                                        ""
                        in
                        if newLbl == oldTransitionName || newLbl == "" then
                            ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

                        else
                            ( ( { model | machineState = Regular }, pModel, sModel ), True, sendMsg <| SaveTransitionName tId newLbl )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == 68 then
                --pressed delete
                case model.machineState of
                    SelectedState stId ->
                        let
                            newDelta =
                                Dict.map (\_ d -> Dict.filter (\tId _ -> not <| Dict.member tId removedTransitions) d) oldMachine.delta

                            newMachine =
                                { oldMachine
                                    | q = Set.remove stId oldMachine.q
                                    , delta = newDelta
                                    , statePositions = Dict.remove stId oldMachine.statePositions
                                    , stateTransitions = newStateTransitions
                                    , stateNames = Dict.remove stId oldMachine.stateNames
                                    , transitionNames = Dict.diff oldMachine.transitionNames removedTransitions
                                }

                            newStateTransitions =
                                Dict.filter (\( _, t, _ ) _ -> not <| Dict.member t removedTransitions) oldMachine.stateTransitions

                            removedTransitions =
                                Dict.fromList <| List.map (\( _, t, _ ) -> ( t, () )) <| Dict.keys <| Dict.filter (\( s0, _, s1 ) _ -> s0 == stId || s1 == stId) oldMachine.stateTransitions
                        in
                        ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                    SelectedArrow ( _, tId, _ ) ->
                        let
                            newDelta =
                                Dict.map (\_ d -> Dict.filter (\tId0 _ -> tId /= tId0) d) oldMachine.delta

                            newMachine =
                                { oldMachine
                                    | delta = newDelta
                                    , stateTransitions = newStateTransitions
                                    , transitionNames = Dict.remove tId oldMachine.transitionNames
                                }

                            newStateTransitions =
                                Dict.filter (\( _, tId0, _ ) _ -> tId /= tId0) oldMachine.stateTransitions
                        in
                        ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else
                case model.machineState of
                    SelectedState sId ->
                        if k == 70 then
                            let
                                newMachine =
                                    { oldMachine
                                        | final =
                                            case Set.member sId oldMachine.final of
                                                True ->
                                                    Set.remove sId oldMachine.final

                                                False ->
                                                    Set.insert sId oldMachine.final
                                    }
                            in
                            ( ( model, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                        else
                            ( ( model, pModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

        SaveStateName sId newLbl ->
            let
                newMachine =
                    { oldMachine | stateNames = Dict.insert sId newLbl oldMachine.stateNames }
            in
            ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

        SaveTransitionName tId newLbl ->
            let
                newMachine =
                    { oldMachine | transitionNames = Dict.insert tId (parseString2Set newLbl) oldMachine.transitionNames }
            in
            ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

        NoOp ->
            ( ( model, pModel, sModel ), False, Cmd.none )


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        winX =
            toFloat <| first env.windowSize

        winY =
            toFloat <| second env.windowSize
    in
    group
        [ rect winX winY
            |> filled blank
            |> (if env.holdingShift then
                    notifyTapAt AddState

                else
                    case model.machineState of
                        SelectedState _ ->
                            notifyTap (MachineMsg Reset)

                        SelectedArrow _ ->
                            notifyTap (MachineMsg Reset)

                        _ ->
                            identity
               )
        , GraphicSVG.map MachineMsg <| Machine.view env model.machineState sModel.machine Set.empty
        ]


updateStatePos : StateID -> ( Float, Float ) -> StatePositions -> StatePositions
updateStatePos st ( x, y ) pos =
    Dict.update st
        (\m ->
            case m of
                Just _ ->
                    Just ( x, y )

                Nothing ->
                    Nothing
        )
        pos


updateArrowPos : StateID -> Float -> StateTransitions -> StateTransitions
updateArrowPos st angle pos =
    Dict.map
        (\( st0, char, st1 ) ( x, y ) ->
            if st0 == st then
                ( x * cos angle, y * sin angle )

            else if st1 == st then
                ( x * cos -angle, y * sin -angle )

            else
                ( x, y )
        )
        pos


editingButtons =
    group
        [ icon (group [ rect 20 3 |> filled black, rect 3 20 |> filled black ])
            |> move ( 30, -30 )
        ]


icon sh =
    group
        [ circle 20 |> filled (rgb 220 220 220)
        , sh
        ]
