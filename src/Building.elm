module Building exposing (..)
import Machine exposing(..)
import Dict exposing (Dict)
import Environment exposing (Environment)
import SharedModel exposing (SharedModel)

type alias Model
    = 
    {
        machineState : Machine.Model
    }

type PersistentModel =
    Empty

type Msg
    = MachineMsg Machine.Msg

update : Environment -> Msg -> (Model, PersistentModel, SharedModel) -> ( (Model, PersistentModel, SharedModel), Bool, Cmd Msg )
update msg model =
    let
        oldMachine =
            model.machine
    in
    case msg of
        StartDragging st ( x, y ) ->
            let
                ( sx, sy ) =
                    case Dict.get st oldMachine.statePositions of
                        Just ( xx, yy ) ->
                            ( xx, yy )

                        Nothing ->
                            ( 0, 0 )
            in
            case model of
                MousingOverRim sId _ ->
                    ( { model | appState = AddingArrow sId ( x, y ) }
                    , Cmd.none
                    )

                _ ->
                    ( { model | appState = DraggingState st ( x - sx, y - sy ) }, Cmd.none )

        StartDraggingArrow ( st1, char, st2 ) ->
            ( { model | appState = DraggingArrow ( st1, char, st2 ) }, Cmd.none )

        StartMouseOverRim stId ( x, y ) ->
            case model.appState of
                Regular ->
                    ( { model | appState = MousingOverRim stId ( x, y ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveMouseOverRim ( x, y ) ->
            case model.appState of
                (MousingOverRim stId _) ->
                    ( { model | appState = MousingOverRim stId ( x, y ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopMouseOverRim ->
            ( { model | appState = Regular }, Cmd.none )

        StopDragging ->
            case model.appState of
                (DraggingState st _) ->
                    ( { model | appState = (SelectedState st) }, Cmd.none )

                (AddingArrowOverOtherState st _ s1) ->
                    let
                        newTrans =
                            case List.head <| Dict.values oldMachine.transitionNames of
                                Just char ->
                                    char

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
                    ( { model
                        | appState = Regular
                        , machine =
                            { oldMachine
                                | delta = newDelta
                                , transitionNames = Dict.insert newTransID newTrans oldMachine.transitionNames
                                , stateTransitions = Dict.insert ( st, newTransID, s1 ) ( 0, 0 ) oldMachine.stateTransitions
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | appState = Regular }, Cmd.none )

        SelectArrow ( s0, char, s1 ) ->
            ( { model | appState = SelectedArrow ( s0, char, s1 ) }, Cmd.none )

        Drag ( x, y ) ->
            case model.appState of
                (DraggingState st ( ox, oy )) ->
                    let
                        ( sx, sy ) =
                            case Dict.get st oldMachine.statePositions of
                                Just ( xx, yy ) ->
                                    ( xx, yy )

                                Nothing ->
                                    ( 0, 0 )
                    in
                    ( { model
                        | machine = { oldMachine | statePositions = updateStatePos st ( x - ox, y - oy ) oldMachine.statePositions }
                      }
                    , Cmd.none
                    )

                (DraggingArrow ( s1, char, s2 )) ->
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
                    ( { model | machine = { oldMachine | stateTransitions = Dict.insert ( s1, char, s2 ) nprot oldMachine.stateTransitions } }, Cmd.none )

                (AddingArrow st _) ->
                    let
                        aboveStates =
                            List.map (\( sId, _ ) -> sId) <|
                                Dict.toList <|
                                    Dict.filter (\_ ( x1, y1 ) -> (x1 - x) ^ 2 + (y1 - y) ^ 2 <= 400) oldMachine.statePositions

                        newState =
                            case aboveStates of
                                h :: _ ->
                                    if st /= h then
                                        AddingArrowOverOtherState st ( x, y ) h

                                    else
                                        AddingArrow st ( x, y )

                                _ ->
                                    AddingArrow st ( x, y )
                    in
                    ( { model | appState = newState }
                    , Cmd.none
                    )

                (AddingArrowOverOtherState st _ s1) ->
                    let
                        aboveStates =
                            List.map (\( sId, _ ) -> sId) <|
                                Dict.toList <|
                                    Dict.filter (\_ ( x1, y1 ) -> (x1 - x) ^ 2 + (y1 - y) ^ 2 <= 400) oldMachine.statePositions

                        newState =
                            case aboveStates of
                                h :: _ ->
                                    if st /= h then
                                        AddingArrowOverOtherState st ( x, y ) h

                                    else
                                        AddingArrow st ( x, y )

                                _ ->
                                    AddingArrow st ( x, y )
                    in
                    ( { model | appState = newState }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MouseOverStateLabel st ->
            ( { model | appState = MousingOverStateLabel st }, Cmd.none )

        MouseOverTransitionLabel tr ->
            ( { model
                | appState =
                    case model.appState of
                        Regular ->
                            MousingOverTransitionLabel tr

                        _ ->
                            model.appState
              }
            , Cmd.none
            )

        MouseLeaveLabel ->
            ( { model
                | appState =
                    case model.appState of
                        (MousingOverStateLabel _) ->
                            Regular

                        (MousingOverTransitionLabel _) ->
                            Regular

                        _ ->
                            model.appState
              }
            , Cmd.none
            )

        SelectStateLabel st ->
            let
                stateName =
                    case Dict.get st oldMachine.stateNames of
                        Just n ->
                            n

                        Nothing ->
                            ""
            in
            ( { model | appState = EditingStateLabel st stateName }, Cmd.none )

        SelectTransitionLabel tr ->
            let
                transName =
                    case Dict.get tr oldMachine.transitionNames of
                        Just n ->
                            n

                        Nothing ->
                            ""
            in
            ( { model | appState = EditingTransitionLabel tr transName }, Cmd.none )

        EditLabel _ lbl ->
            case model.appState of
                (EditingStateLabel st _) ->
                    ( { model | appState = (EditingStateLabel st lbl) }, Cmd.none )

                (EditingTransitionLabel tr _) ->
                    ( { model | appState = (EditingTransitionLabel tr lbl) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddState ( x, y ) ->
            case model.appState of
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
                    ( { model
                        | machine = newMachine
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | appState = Regular }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

view : Environment -> (Model, PersistentModel, SharedModel) -> Shape Msg
view env (model, pModel, sModel) =
    group
        [
            Machine.view env model.machineState Set.empty 
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