module Building exposing (Model, Msg(..), PersistentModel(..), editingButtons, init, initPModel, onEnter, onExit, subscriptions, update, updateArrowPos, updateStatePos, view)

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
    , snapToGrid : Snap
    }


type Snap
    = SnapToGrid Int
    | NoSnap


type PersistentModel
    = Empty


type Msg
    = MachineMsg Machine.Msg
    | SaveStateName StateID String
    | SaveTransitionName TransitionID String
    | AddState ( Float, Float )
    | KeyPressed Int
    | ToggleSnap
    | ChangeSnap Int
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map KeyPressed (D.field "keyCode" D.int))
        ]


init : Model
init =
    { machineState = Regular
    , snapToGrid = NoSnap
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
                            ( ( { model | machineState = DraggingState st ( x - sx, y - sy ) ( x, y ) }, pModel, sModel ), False, Cmd.none )

                StartDraggingArrow ( st1, char, st2 ) pos ->
                    ( ( { model | machineState = DraggingArrow ( st1, char, st2 ) pos }, pModel, sModel ), False, Cmd.none )

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

                        _ ->
                            ( ( model, pModel, sModel ), False, Cmd.none )

                StopDragging ->
                    case model.machineState of
                        DraggingState st _ _ ->
                            ( ( { model | machineState = SelectedState st }, pModel, sModel ), True, Cmd.none )

                        AddingArrowOverOtherState st _ s1 ->
                            let
                                newTrans =
                                    case List.head <| Dict.values oldMachine.transitionNames of
                                        Just setchar ->
                                            setchar

                                        Nothing ->
                                            Set.singleton "x"

                                newTransID =
                                    case List.maximum <| Dict.keys oldMachine.transitionNames of
                                        Just n ->
                                            n + 1

                                        Nothing ->
                                            0

                                isValidTransition =
                                    checkTransitionValid newTrans

                                oldTransitionMistakes =
                                    oldMachine.transitionMistakes

                                newTransitionMistakes =
                                    if isValidTransition then
                                        case oldTransitionMistakes of
                                            Just setOfMistakes ->
                                                let
                                                    newSetOfMistakes =
                                                        Set.remove newTransID setOfMistakes
                                                in
                                                if Set.isEmpty newSetOfMistakes then
                                                    Nothing

                                                else
                                                    Just newSetOfMistakes

                                            Nothing ->
                                                Nothing

                                    else
                                        case oldTransitionMistakes of
                                            Just setOfMistakes ->
                                                Just <| Set.insert newTransID setOfMistakes

                                            Nothing ->
                                                Just <| Set.singleton newTransID

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

                                newTransPos =
                                    if st == s1 then
                                        ( 0, 50 )

                                    else
                                        ( 0, 0 )
                            in
                            ( ( { model | machineState = Regular }
                              , pModel
                              , { sModel
                                    | machine =
                                        { oldMachine
                                            | delta = newDelta
                                            , transitionNames = Dict.insert newTransID newTrans oldMachine.transitionNames
                                            , stateTransitions = Dict.insert ( st, newTransID, s1 ) newTransPos oldMachine.stateTransitions
                                            , transitionMistakes = newTransitionMistakes
                                        }
                                }
                              )
                            , True
                            , Cmd.none
                            )

                        DraggingArrow tId _ ->
                            ( ( { model | machineState = Regular }, pModel, sModel ), True, Cmd.none )

                        _ ->
                            ( ( { model | machineState = Regular }, pModel, sModel ), False, Cmd.none )

                SelectArrow ( s0, char, s1 ) ->
                    ( ( { model | machineState = SelectedArrow ( s0, char, s1 ) }, pModel, sModel ), False, Cmd.none )

                Drag ( x, y ) ->
                    case model.machineState of
                        DraggingState st ( ox, oy ) _ ->
                            let
                                ( sx, sy ) =
                                    case Dict.get st oldMachine.statePositions of
                                        Just ( xx, yy ) ->
                                            ( xx, yy )

                                        Nothing ->
                                            ( 0, 0 )

                                newPos =
                                    case model.snapToGrid of
                                        SnapToGrid n ->
                                            ( roundTo (toFloat n) (x - ox), roundTo (toFloat n) (y - oy) )

                                        _ ->
                                            ( x - ox, y - oy )
                            in
                            ( ( { model | machineState = DraggingState st ( ox, oy ) ( x, y ) }, pModel, { sModel | machine = { oldMachine | statePositions = updateStatePos st newPos oldMachine.statePositions } } )
                            , False
                            , Cmd.none
                            )

                        DraggingArrow ( s1, char, s2 ) _ ->
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

                                newPos =
                                    case model.snapToGrid of
                                        SnapToGrid n ->
                                            ( roundTo (toFloat n) x, roundTo (toFloat n) y )

                                        _ ->
                                            ( x, y )

                                theta =
                                    -1 * atan2 (y1 - y0) (x1 - x0)

                                ( mx, my ) =
                                    ( (x0 + x1) / 2, (y0 + y1) / 2 )

                                ( nx, ny ) =
                                    sub newPos ( mx, my )

                                nprot =
                                    ( nx * cos theta - ny * sin theta, nx * sin theta + ny * cos theta )
                            in
                            ( ( { model | machineState = DraggingArrow ( s1, char, s2 ) ( x, y ) }, pModel, { sModel | machine = { oldMachine | stateTransitions = Dict.insert ( s1, char, s2 ) nprot oldMachine.stateTransitions } } ), False, Cmd.none )

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
                                    , start = Set.remove stId oldMachine.start
                                    , final = Set.remove stId oldMachine.final
                                    , statePositions = Dict.remove stId oldMachine.statePositions
                                    , stateTransitions = newStateTransitions
                                    , stateNames = Dict.remove stId oldMachine.stateNames
                                    , transitionNames = Dict.diff oldMachine.transitionNames removedTransitions
                                    , transitionMistakes = newTMistakes
                                }

                            newStateTransitions =
                                Dict.filter (\( _, t, _ ) _ -> not <| Dict.member t removedTransitions) oldMachine.stateTransitions

                            newTMistakes =
                                List.foldr (\tId mistakes -> tMistakeRemove (first tId) mistakes) oldMachine.transitionMistakes removedTransitionsLst

                            removedTransitionsLst =
                                List.map (\( _, t, _ ) -> ( t, () )) <| Dict.keys <| Dict.filter (\( s0, _, s1 ) _ -> s0 == stId || s1 == stId) oldMachine.stateTransitions

                            removedTransitions =
                                Dict.fromList removedTransitionsLst
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
                                    , transitionMistakes = newTMistakes
                                }

                            newStateTransitions =
                                Dict.filter (\( _, tId0, _ ) _ -> tId /= tId0) oldMachine.stateTransitions

                            newTMistakes =
                                tMistakeRemove tId oldMachine.transitionMistakes
                        in
                        ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == 71 then
                --pressed G
                ( ( model, pModel, sModel ), False, sendMsg ToggleSnap )

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
                newTransitions =
                    parseString2Set newLbl

                isValidTransition =
                    checkTransitionValid newTransitions

                oldTransitionMistakes =
                    oldMachine.transitionMistakes

                newTransitionMistakes =
                    if isValidTransition then
                        tMistakeRemove tId oldTransitionMistakes

                    else
                        tMistakeAdd tId oldTransitionMistakes

                newMachine =
                    { oldMachine
                        | transitionNames = Dict.insert tId newTransitions oldMachine.transitionNames
                        , transitionMistakes = newTransitionMistakes
                    }
            in
            ( ( { model | machineState = Regular }, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

        ToggleSnap ->
            ( ( { model
                    | snapToGrid =
                        if model.snapToGrid == NoSnap then
                            SnapToGrid 10

                        else
                            NoSnap
                }
              , pModel
              , sModel
              )
            , False
            , Cmd.none
            )

        ChangeSnap nn ->
            ( ( { model
                    | snapToGrid =
                        case model.snapToGrid of
                            SnapToGrid n ->
                                SnapToGrid (n + nn)

                            NoSnap ->
                                NoSnap
                }
              , pModel
              , sModel
              )
            , False
            , Cmd.none
            )

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
        , case ( model.machineState, model.snapToGrid ) of
            ( DraggingState _ ( ox, oy ) ( x, y ), SnapToGrid n ) ->
                group
                    [ graphPaperCustom (toFloat n) 1 gray
                        |> clip (circle 30 |> ghost |> move ( x - ox, y - oy ))
                    , circle 3 |> filled (rgb 112 190 255) |> move ( roundTo 10 (x - ox), roundTo 10 (y - oy) )
                    ]

            ( DraggingArrow id pos, SnapToGrid n ) ->
                group
                    [ graphPaperCustom (toFloat n) 1 gray
                        |> clip (circle 30 |> ghost |> move pos)
                    ]

            _ ->
                group []
        , GraphicSVG.map MachineMsg <| Machine.view env model.machineState sModel.machine Set.empty
        , editingButtons model |> move ( winX / 2 - 30, -winY / 2 + 25 )
        ]


roundTo : Float -> Float -> Float
roundTo n m =
    toFloat (round (m + n / 2) // round n * round n)


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


editingButtons model =
    let
        snapping =
            case model.snapToGrid of
                SnapToGrid _ ->
                    True

                _ ->
                    False
    in
    group
        [ icon snapping
            (snapIcon
                |> scale 0.75
                |> repaint
                    (if snapping then
                        white

                     else
                        gray
                    )
            )
            |> notifyTap ToggleSnap
            |> move ( -36, 0 )
        ]


snapIcon =
    group
        [ group
            [ roundedRect 33 4 2.5 |> filled black |> move ( 0, 10 )
            , roundedRect 33 4 2.5 |> filled black
            , roundedRect 33 4 2.5 |> filled black |> move ( 0, -10 )
            , roundedRect 4 33 2.5 |> filled black |> move ( 10, 0 )
            , roundedRect 4 33 2.5 |> filled black
            , roundedRect 4 33 2.5 |> filled black |> move ( -10, 0 )
            ]
            |> subtract
                (group
                    [ wedge 10 0.5 |> ghost |> rotate (degrees 90)
                    , rect 8 12 |> ghost |> move ( 6, -6 )
                    , rect 8 12 |> ghost |> move ( -6, -6 )
                    , rect 12 8 |> ghost |> move ( 0, -3 )
                    ]
                    |> move ( 5, -10 )
                )
        , group
            [ wedge 8 0.5
                |> filled black
                |> rotate (degrees 90)
                |> subtract (wedge 2 0.5 |> ghost |> rotate (degrees 90))
            , rect 6 6
                |> filled black
                |> move ( 5, -3 )
                |> subtract (rect 2.5 3 |> ghost |> move ( 5, -3 ))
            , rect 6 6
                |> filled black
                |> move ( -5, -3 )
                |> subtract (rect 2.5 3 |> ghost |> move ( -5, -3 ))
            ]
            |> move ( 5, -10 )
        ]


checkTransitionValid : Set.Set String -> Bool
checkTransitionValid set =
    case Set.member "\\epsilon" set of
        False ->
            True

        True ->
            if Set.size set == 1 then
                True

            else
                False
