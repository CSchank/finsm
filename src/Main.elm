module Main exposing (ApplicationState(..), BuildingMsg(..), BuildingState(..), Character, Delta, InputTape, LatexAlign(..), Machine, Model, Msg(..), SimulateData, SimulatingMsg(..), SimulatingState(..), StateID, StateNames, StatePositions, StateTransitions, TransitionID, TransitionNames, add, arrow, buildingUpdate, delta, deltaHat, dot, editIcon, editingButtons, icon, initSimData, isAccept, latex, latexKeyboard, latexurl, main, modeButtons, mult, p, renderArrow, renderArrows, renderSimulate, renderStates, renderTape, setMax, simulatingUpdate, sub, test, textBox, textHtml, trashIcon, update, updateArrowPos, updateStatePos, vertex, view)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import GraphicSVG exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode
import List
import Random
import Set exposing (Set)
import Task
import Tuple exposing (first, second)
import Url exposing (Url, percentEncode)


type Msg
    = GoTo ApplicationState
    | BMsg BuildingMsg
    | SMsg SimulatingMsg
    | KeyPressed Int
    | KeyReleased Int
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest


type BuildingMsg
    = StartDragging StateID ( Float, Float )
    | StartDraggingArrow ( StateID, TransitionID, StateID )
    | StartMouseOverRim StateID ( Float, Float )
    | MoveMouseOverRim ( Float, Float )
    | StopMouseOverRim
    | SelectArrow ( StateID, TransitionID, StateID )
    | MouseOverStateLabel StateID
    | MouseOverTransitionLabel TransitionID
    | MouseLeaveLabel
    | SelectStateLabel StateID
    | SelectTransitionLabel StateID
    | EditLabel StateID String
    | Drag ( Float, Float )
    | AddState ( Float, Float )
    | StopDragging


type SimulatingMsg
    = Step
    | EditTape Int
    | DeleteTape Int
    | AddNewTape
    | ChangeTape Int
    | ToggleStart StateID


type alias StateID =
    Int


type alias TransitionID =
    Int


type alias Character =
    String


type alias Delta =
    Dict StateID (Dict TransitionID StateID)


type alias InputTape =
    Array Character


type alias StatePositions =
    Dict StateID ( Float, Float )


type alias StateNames =
    Dict StateID String


type alias TransitionNames =
    Dict TransitionID String


type alias StateTransitions =
    Dict ( StateID, TransitionID, StateID ) ( Float, Float )


type ApplicationState
    = Building BuildingState
    | Simulating SimulatingState


type BuildingState
    = Regular
    | DraggingState StateID ( Float, Float )
    | SelectedState StateID
    | MousingOverRim StateID ( Float, Float )
    | AddingArrow StateID ( Float, Float )
    | AddingArrowOverOtherState StateID ( Float, Float ) StateID
    | MousingOverStateLabel StateID
    | MousingOverTransitionLabel TransitionID
    | EditingStateLabel StateID String
    | EditingTransitionLabel TransitionID String
    | SelectedArrow ( StateID, TransitionID, StateID )
    | DraggingArrow ( StateID, TransitionID, StateID )
    | CreatingNewArrow StateID



{- source StateID -}


type SimulatingState
    = SimRegular Int {- tapeID -} Int {- charID -}
    | SimEditing Int



{- tapeID -}


type alias SimulateData =
    { tapes : Dict Int (Array Character)
    }


initSimData =
    { tapes =
        Dict.fromList
            [ ( 0, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ] )
            , ( 1, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0" ] )
            ]
    }


type alias Model =
    { appState : ApplicationState
    , simulateData : SimulateData
    , machine : Machine
    , states : Set StateID
    , windowSize : ( Int, Int )
    , holdingShift : Bool
    }


type alias Machine =
    { q : Set StateID
    , delta : Delta
    , start : Set StateID
    , final : Set StateID
    , statePositions : StatePositions
    , stateTransitions : StateTransitions
    , stateNames : StateNames
    , transitionNames : TransitionNames
    }


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


test : Machine
test =
    let
        q =
            Set.fromList [ 0, 1, 2, 3 ]

        delta0 =
            Dict.fromList
                [ ( 0, Dict.fromList [ ( 0, 1 ), ( 1, 2 ) ] )
                , ( 1, Dict.fromList [ ( 2, 0 ), ( 3, 3 ) ] )
                , ( 2, Dict.fromList [ ( 4, 3 ), ( 5, 0 ) ] )
                , ( 3, Dict.fromList [ ( 6, 2 ), ( 7, 1 ) ] )
                ]

        start =
            Set.fromList [ 0 ]

        final =
            Set.fromList [ 0 ]

        statePositions = Dict.fromList [ ( 0, ( -50, 50 ) ), ( 1, ( 50, 50 ) ), ( 2, ( -50, -50 ) ), ( 3, ( 50, -50 ) ) ]
        stateNames = Dict.fromList [ ( 0, "q_0" ), ( 1, "q_1" ), ( 2, "q_2" ), ( 3, "q_3" ) ]
        transitionNames = Dict.fromList [ ( 0, "1" ), ( 1, "0" ), ( 2, "1" ), ( 3, "0" ), ( 4, "1" ), ( 5, "0" ), ( 6, "1" ), ( 7, "0" ), ( 8, "1" ) ]
        stateTransitions =
            Dict.fromList
                [ ( ( 0, 0, 1 ), ( 0, 10 ) )
                , ( ( 1, 2, 0 ), ( 0, 10 ) )
                , ( ( 0, 1, 2 ), ( 0, 10 ) )
                , ( ( 2, 5, 0 ), ( 0, 10 ) )
                , ( ( 2, 4, 3 ), ( 0, 10 ) )
                , ( ( 3, 6, 2 ), ( 0, 10 ) )
                , ( ( 1, 3, 3 ), ( 0, 10 ) )
                , ( ( 3, 7, 1 ), ( 0, 10 ) )
                ]
    in
        Machine q delta0 start final statePositions stateTransitions stateNames transitionNames 


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appState = Building Regular
                  , machine = test
                  , simulateData = initSimData
                  , states = test.start
                  
                  , windowSize = ( 0, 0 )
                  , holdingShift = False
                  }
                , Task.perform (\vp -> WindowSize ( round vp.viewport.width, round vp.viewport.height )) Browser.Dom.getViewport
                )
        , update = update
        , view = \m -> { body = view m, title = "finSM - create and simulate finite state machines" }
        , subscriptions =
            \model ->
                Sub.batch
                    [ Browser.Events.onResize (\w h -> WindowSize ( w, h ))
                    , Browser.Events.onKeyDown (D.map KeyPressed (D.field "keyCode" D.int))
                    , Browser.Events.onKeyUp (D.map KeyReleased (D.field "keyCode" D.int))
                    ]
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


update msg model =
    let 
        oldMachine = model.machine
    in case msg of
        GoTo state ->
            ( { model | appState = state }, Cmd.none )

        BMsg bmsg ->
            case model.appState of
                Building _ ->
                    buildingUpdate bmsg model

                _ ->
                    ( model, Cmd.none )

        SMsg smsg ->
            case model.appState of
                Simulating _ ->
                    simulatingUpdate smsg model

                _ ->
                    ( model, Cmd.none )

        WindowSize ( w, h ) ->
            ( { model | windowSize = ( w, h ) }, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        KeyReleased k ->
            if k == 16 then
                ( { model | holdingShift = False }, Cmd.none )

            else
                ( model, Cmd.none )

        KeyPressed k ->
            if k == 13 then
                --pressed enter
                case model.appState of
                    Building (EditingStateLabel stId newLbl) ->
                        let
                            oldStateName =
                                case Dict.get stId oldMachine.stateNames of
                                    Just n ->
                                        n

                                    _ ->
                                        ""
                        in
                        if newLbl == oldStateName || newLbl == "" then
                            ( { model | appState = Building Regular }, Cmd.none )

                        else
                            ( { model
                                | machine = { oldMachine | stateNames = Dict.insert stId newLbl oldMachine.stateNames }
                                , appState = Building Regular
                              }
                            , Cmd.none
                            )

                    Building (EditingTransitionLabel tId newLbl) ->
                        let
                            oldTransitionName =
                                case Dict.get tId oldMachine.transitionNames of
                                    Just n ->
                                        n

                                    _ ->
                                        ""
                        in
                        if newLbl == oldTransitionName || newLbl == "" then
                            ( { model | appState = Building Regular }, Cmd.none )

                        else
                            ( { model
                                | machine = { oldMachine | transitionNames = Dict.insert tId newLbl oldMachine.transitionNames}
                                , appState = Building Regular
                              }
                            , Cmd.none
                            )

                    Simulating (SimEditing tId) ->
                        ( { model | appState = Simulating (SimRegular tId -1) }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else if k == 8 then
                --pressed delete
                case model.appState of
                    Building (SelectedState stId) ->
                        let
                            newDelta =
                                Dict.map (\_ d -> Dict.filter (\tId _ -> not <| Dict.member tId removedTransitions) d) oldMachine.delta

                            newMachine =
                                { oldMachine | q = Set.remove stId oldMachine.q, delta = newDelta 
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
                        ( { model
                            | machine = newMachine
                            , appState = Building Regular
                          }
                        , Cmd.none
                        )

                    Building (SelectedArrow ( _, tId, _ )) ->
                        let

                            newDelta =
                                Dict.map (\_ d -> Dict.filter (\tId0 _ -> tId /= tId0) d) oldMachine.delta

                            newMachine =
                                { oldMachine | delta = newDelta
                                , stateTransitions = newStateTransitions
                                , transitionNames = Dict.remove tId oldMachine.transitionNames }

                            newStateTransitions =
                                Dict.filter (\( _, tId0, _ ) _ -> tId /= tId0) oldMachine.stateTransitions
                        in
                        ( { model
                            | machine = newMachine
                            , appState = Building Regular
                            
                          }
                        , Cmd.none
                        )

                    Simulating (SimEditing tapeId) ->
                        let
                            oldSimData =
                                model.simulateData
                        in
                        ( { model
                            | simulateData =
                                { oldSimData
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case m of
                                                    Just ar ->
                                                        Just <| Array.slice 0 -1 ar

                                                    _ ->
                                                        m
                                            )
                                            oldSimData.tapes
                                }
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

            else if k == 39 then
                --right arrow key
                case model.appState of
                    Simulating (SimRegular tapeId _) ->
                        ( model, Task.perform identity (Task.succeed <| SMsg Step) )

                    _ ->
                        ( model, Cmd.none )

            else if k == 16 then
                ( { model | holdingShift = True }, Cmd.none )

            else
                case model.appState of
                    Simulating (SimEditing tapeId) ->
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

                            oldSimData =
                                model.simulateData
                        in
                        ( { model
                            | simulateData =
                                { oldSimData
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
                                            oldSimData.tapes
                                }
                          }
                        , Cmd.none
                        )

                    Building (SelectedState sId) ->
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
                            ( { model | machine = newMachine }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )


buildingUpdate : BuildingMsg -> Model -> ( Model, Cmd Msg )
buildingUpdate msg model =
    let
        oldMachine = model.machine
    in case msg of
        StartDragging st ( x, y ) ->
            let
                ( sx, sy ) =
                    case Dict.get st oldMachine.statePositions of
                        Just ( xx, yy ) ->
                            ( xx, yy )

                        Nothing ->
                            ( 0, 0 )
            in
            case model.appState of
                Building (MousingOverRim sId _) ->
                    ( { model | appState = Building <| AddingArrow sId ( x, y ) }
                    , Cmd.none
                    )

                _ ->
                    ( { model | appState = Building <| DraggingState st ( x - sx, y - sy ) }, Cmd.none )

        StartDraggingArrow ( st1, char, st2 ) ->
            ( { model | appState = Building <| DraggingArrow ( st1, char, st2 ) }, Cmd.none )

        StartMouseOverRim stId ( x, y ) ->
            case model.appState of
                Building Regular ->
                    ( { model | appState = Building <| MousingOverRim stId ( x, y ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MoveMouseOverRim ( x, y ) ->
            case model.appState of
                Building (MousingOverRim stId _) ->
                    ( { model | appState = Building <| MousingOverRim stId ( x, y ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopMouseOverRim ->
            ( { model | appState = Building Regular }, Cmd.none )

        StopDragging ->
            case model.appState of
                Building (DraggingState st _) ->
                    ( { model | appState = Building (SelectedState st) }, Cmd.none )

                Building (AddingArrowOverOtherState st _ s1) ->
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
                        | appState = Building Regular
                        , machine = { oldMachine | delta = newDelta 
                        , transitionNames = Dict.insert newTransID newTrans oldMachine.transitionNames
                        , stateTransitions = Dict.insert ( st, newTransID, s1 ) ( 0, 0 ) oldMachine.stateTransitions
                        }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | appState = Building Regular }, Cmd.none )

        SelectArrow ( s0, char, s1 ) ->
            ( { model | appState = Building <| SelectedArrow ( s0, char, s1 ) }, Cmd.none )

        Drag ( x, y ) ->
            case model.appState of
                Building (DraggingState st ( ox, oy )) ->
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

                Building (DraggingArrow ( s1, char, s2 )) ->
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

                Building (AddingArrow st _) ->
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
                    ( { model | appState = Building newState }
                    , Cmd.none
                    )

                Building (AddingArrowOverOtherState st _ s1) ->
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
                    ( { model | appState = Building newState }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MouseOverStateLabel st ->
            ( { model | appState = Building <| MousingOverStateLabel st }, Cmd.none )

        MouseOverTransitionLabel tr ->
            ( { model
                | appState =
                    case model.appState of
                        Building Regular ->
                            Building <| MousingOverTransitionLabel tr

                        _ ->
                            model.appState
              }
            , Cmd.none
            )

        MouseLeaveLabel ->
            ( { model
                | appState =
                    case model.appState of
                        Building (MousingOverStateLabel _) ->
                            Building Regular

                        Building (MousingOverTransitionLabel _) ->
                            Building Regular

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
            ( { model | appState = Building <| EditingStateLabel st stateName }, Cmd.none )

        SelectTransitionLabel tr ->
            let
                transName =
                    case Dict.get tr oldMachine.transitionNames of
                        Just n ->
                            n

                        Nothing ->
                            ""
            in
            ( { model | appState = Building <| EditingTransitionLabel tr transName }, Cmd.none )

        EditLabel _ lbl ->
            case model.appState of
                Building (EditingStateLabel st _) ->
                    ( { model | appState = Building (EditingStateLabel st lbl) }, Cmd.none )

                Building (EditingTransitionLabel tr _) ->
                    ( { model | appState = Building (EditingTransitionLabel tr lbl) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddState ( x, y ) ->
            case model.appState of
                Building Regular ->
                    let
                        newId =
                            setMax oldMachine.q + 1

                        newMachine =
                            { oldMachine | q = Set.insert newId oldMachine.q
                            , statePositions = Dict.insert newId ( x, y ) oldMachine.statePositions
                            , stateNames = Dict.insert newId ("q_{" ++ String.fromInt newId ++ "}") oldMachine.stateNames}
                    in
                    ( { model
                        | machine = newMachine
                      }
                    , Cmd.none
                    )

                Building _ ->
                    ( { model | appState = Building Regular }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


simulatingUpdate : SimulatingMsg -> Model -> ( Model, Cmd Msg )
simulatingUpdate msg model =
    let
        oldMachine = model.machine
    in case msg of
        Step ->
            case model.appState of
                Simulating (SimRegular tapeId charId) ->
                    let
                        nextCh =
                            case Dict.get tapeId model.simulateData.tapes of
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
                        ( { model
                            | states = deltaHat oldMachine.transitionNames oldMachine.delta nextCh model.states
                            , appState = Simulating (SimRegular tapeId (charId + 1))
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTape tId ->
            ( { model | appState = Simulating (SimEditing tId) }, Cmd.none )

        DeleteTape tId ->
            let
                oldSimData =
                    model.simulateData
            in
            ( { model | simulateData = { oldSimData | tapes = Dict.remove tId oldSimData.tapes } }, Cmd.none )

        AddNewTape ->
            let
                oldSimData =
                    model.simulateData

                newId =
                    (case List.maximum <| Dict.keys model.simulateData.tapes of
                        Just n ->
                            n

                        Nothing ->
                            0
                    )
                        + 1
            in
            ( { model | simulateData = { oldSimData | tapes = Dict.insert newId Array.empty oldSimData.tapes } }, Cmd.none )

        ChangeTape tId ->
            ( { model
                | states = oldMachine.start
                , appState = Simulating (SimRegular tId -1)
              }
            , Cmd.none
            )

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
            case model.appState of
                Simulating (SimRegular tapeId _) ->
                    ( { model
                        | machine = newMachine
                        , appState = Simulating (SimRegular tapeId 0)
                        , states = newMachine.start
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


setMax : Set Int -> Int
setMax s =
    Set.foldl max 0 s



--renameState : State -> String ->


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


isAccept : Set StateID -> Set StateID -> InputTape -> Int -> Bool
isAccept states finals input inputAt =
    if inputAt == Array.length input then
        Set.size (Set.intersect states finals) > 0

    else
        False


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
                        |> notifyTap (SMsg <| ChangeTape tapeId)
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
                        |> notifyTap (SMsg <| EditTape tapeId)
                    , group
                        [ roundedRect 15 15 2
                            |> filled white
                            |> addOutline (solid 1) darkGray
                        , trashIcon |> scale 0.2 |> move ( 0, -1 )
                        ]
                        |> move ( toFloat <| (Array.length input + 1) * xpad, 3 )
                        |> notifyTap (SMsg <| DeleteTape tapeId)
                    ]

                else
                    []
               )



{-
   |> move
       ( -xpad / 2 * toFloat (Array.length input - 1)
       , 0
       )
-}
--renderStates : Set State -> StatePositions


textHtml : String -> Html msg
textHtml t =
    H.span
        [ Json.Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


editIcon =
    group
        [ --square 5 |> outlined (solid 1) black
          rect 5 2
            |> filled (rgb 21 137 255)
            |> rotate (degrees 45)
            |> move ( 3, 3 )
        , triangle 1
            |> filled blue
            |> rotate (degrees -15)
        ]


trashIcon =
    group
        [ roundedRect 30 40 3
            |> outlined (solid 4) black
        , rect 42 5 |> filled black |> move ( 0, 19.5 )
        , roundedRect 36 5 1 |> filled black |> move ( 0, 21.5 )
        , roundedRect 10 10 1 |> outlined (solid 3) black |> move ( 0, 23.5 )
        , rect 4 30 |> filled black
        , rect 4 30 |> filled black |> move ( -8, 0 )
        , rect 4 30 |> filled black |> move ( 8, 0 )
        ]


renderStates : Set StateID -> Set StateID -> Set StateID -> StatePositions -> Model -> Shape Msg
renderStates states currents finals pos model =
    let
        oldMachine = model.machine

        stateList =
            Set.toList states

        getPos state =
            case Dict.get state pos of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )

        thickness state =
            case model.appState of
                Simulating _ ->
                    if Set.member state currents then
                        2

                    else
                        1

                _ ->
                    1

        stateName sId =
            case Dict.get sId oldMachine.stateNames of
                Just n ->
                    n

                _ ->
                    ""
    in
    group <|
        List.map
            (\sId ->
                group
                    [ circle 21
                        |> outlined (solid 3) blank
                        |> notifyEnterAt (BMsg << StartMouseOverRim sId)
                    , circle 20
                        |> outlined (solid (thickness sId)) black
                        |> notifyMouseDownAt (BMsg << StartDragging sId)
                    , if Set.member sId finals then
                        circle 17
                            |> outlined (solid (thickness sId)) black
                            |> notifyMouseDownAt (BMsg << StartDragging sId)

                      else
                        group []
                    , case model.appState of
                        Building (SelectedState st) ->
                            if st == sId then
                                circle 20.75
                                    |> outlined (solid 1.5) lightBlue
                                    |> notifyMouseDownAt (BMsg << StartDragging sId)

                            else
                                group []

                        Building (MousingOverRim st ( x, y )) ->
                            let
                                ( x0, y0 ) =
                                    getPos st

                                ( dx, dy ) =
                                    ( x - x0, y - y0 )
                            in
                            if st == sId then
                                group
                                    [ circle 7
                                        |> filled white
                                        |> addOutline (solid 0.5) black
                                    , rect 8 1.5 |> filled black
                                    , rect 1.5 8 |> filled black
                                    ]
                                    |> notifyMouseMoveAt (BMsg << MoveMouseOverRim)
                                    |> notifyMouseDownAt (BMsg << StartDragging sId)
                                    |> notifyLeave (BMsg StopMouseOverRim)
                                    |> move ( 20 * cos (atan2 dy dx), 20 * sin (atan2 dy dx) )

                            else
                                group []

                        Building (AddingArrowOverOtherState _ _ st) ->
                            if st == sId then
                                circle 21.5
                                    |> outlined (solid 3) (rgb 112 190 255)
                                    |> notifyMouseDownAt (BMsg << StartDragging sId)
                                    |> notifyLeave (BMsg StopMouseOverRim)

                            else
                                group []

                        _ ->
                            group []
                    , case model.appState of
                        Building (MousingOverStateLabel st) ->
                            if sId == st then
                                editIcon |> scale 0.75 |> move ( 5, 5.5 )

                            else
                                group []

                        _ ->
                            group []
                    , case model.appState of
                        Building (EditingStateLabel st str) ->
                            if st == sId then
                                textBox str
                                    (if String.length str == 0 then
                                        34

                                     else
                                        6 * toFloat (String.length str)
                                    )
                                    20
                                    "LaTeX"
                                    (BMsg << EditLabel sId)

                            else
                                group
                                    [ latex 25 18 (stateName sId) AlignCentre
                                        |> move ( 0, 9 )
                                    , rect 25 18
                                        |> filled blank
                                        |> notifyEnter (BMsg <| MouseOverStateLabel sId)
                                        |> notifyLeave (BMsg MouseLeaveLabel)
                                        |> notifyTap (BMsg <| SelectStateLabel sId)
                                    ]

                        _ ->
                            group
                                [ latex 25 18 (stateName sId) AlignCentre
                                    |> move ( 0, 9 )
                                , rect 25 18
                                    |> filled blank
                                    |> notifyEnter (BMsg <| MouseOverStateLabel sId)
                                    |> notifyLeave (BMsg MouseLeaveLabel)
                                    |> notifyTap (BMsg <| SelectStateLabel sId)
                                ]

                    --, text state |> centered |> filled black |> move ( 0, -3 )
                    {--, case model.appState of
                            DraggingState st ->
                                if st == state then
                                    circle 21 |> outlined (solid 1) lightBlue
                                else
                                    group []

                            a ->
                                group []--}
                    ]
                    |> move (getPos sId)
                    |> (case model.appState of
                            Simulating _ ->
                                notifyTap (SMsg <| ToggleStart sId)

                            _ ->
                                identity
                       )
            )
            stateList


textBox : String -> Float -> Float -> String -> (String -> Msg) -> Shape Msg
textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ placeholder place
                , onInput msg
                , value txt
                , style "width" (String.fromFloat w ++ "px")
                , style "height" (String.fromFloat h ++ "px")
                , style "margin-top" "1px"
                , style "font-family" "monospace"
                ]
                []


type LatexAlign
    = AlignLeft
    | AlignRight
    | AlignCentre


latex w h txt align =
    (html w h <|
        H.div
            [ style "width" "100%"
            , style "height" "100%"
            , attribute "moz-user-select" "none"
            , attribute "webkit-user-select" "none"
            , attribute "user-select" "none"
            ]
            [ H.img
                ([ Html.Attributes.attribute "onerror" ("this.src='" ++ latexurl "\\LaTeX?" ++ "'")
                 , Html.Attributes.src (latexurl txt)

                 --, style "width" "100%"
                 , style "height" "100%"
                 ]
                    ++ (case align of
                            AlignCentre ->
                                [ style "margin-left" "auto"
                                , style "margin-right" "auto"
                                ]

                            AlignLeft ->
                                [ style "margin-right" "auto"
                                ]

                            AlignRight ->
                                [ style "margin-left" "auto"
                                ]
                       )
                    ++ [ style "display" "block"
                       , style "max-width" "100%"
                       ]
                )
                []
            ]
    )
        |> move ( -w / 2, 0 )


latexurl : String -> String
latexurl lx =
    "https://finsm.io/latex/render/" ++ percentEncode lx



--"http://localhost:8001/latex/render/" ++ percentEncode lx


arrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        ( dx, dy ) =
            ( x2 - x1, y2 - y1 )
    in
    group
        [ curve ( x0, y0 )
            [ Pull ( x1, y1 )
                ( x2 - 2 * cos (atan2 dy dx)
                , y2 - 2 * sin (atan2 dy dx)
                )
            ]
            |> outlined (solid 1) black
        , triangle 4
            |> filled black
            |> rotate (atan2 dy dx)
            |> move ( x2 - 4 * cos (atan2 dy dx), y2 - 4 * sin (atan2 dy dx) )
        ]


renderArrow : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> Character -> TransitionID -> Bool -> StateID -> StateID -> ApplicationState -> Shape Msg
renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) r0 r1 char charID sel s1 s2 appState =
    let
        ( tx, ty ) =
            --tangent between to and from states
            ( x2 - x0, y2 - y0 )

        theta =
            atan2 ty tx

        ( rx, ry ) =
            ( x1 * cos theta - y1 * sin theta, y1 * cos theta + x1 * sin theta )

        ( mx, my ) =
            --pull point
            ( (x2 + x0) / 2 + rx, (y2 + y0) / 2 + ry )

        ( dx0, dy0 ) =
            --tangent from middle point to from state
            ( mx - x0, my - y0 )

        ( dx1, dy1 ) =
            --tangent from middle point to to state
            ( mx - x2, my - y2 )

        ( xx0, yy0 ) =
            --from state position (with radius accounted for)
            ( x0 + r0 * cos (atan2 dy0 dx0), y0 + r0 * sin (atan2 dy0 dx0) )

        ( xx1, yy1 ) =
            --to state position (with radius accounted for)
            ( x2 + r1 * cos (atan2 dy1 dx1), y2 + r1 * sin (atan2 dy1 dx1) )

        offset =
            if y1 > 0 then
                8

            else
                -8
    in
    group
        [ group
            [ arrow ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 )
                |> notifyMouseDown (BMsg <| SelectArrow ( s1, charID, s2 ))
            , group
                [ case appState of
                    Building (EditingTransitionLabel tId str) ->
                        if tId == charID then
                            textBox str
                                (if String.length str == 0 then
                                    34

                                 else
                                    6 * toFloat (String.length str)
                                )
                                20
                                "LaTeX"
                                (BMsg << EditLabel tId)

                        else
                            latex 50 12 char AlignCentre

                    _ ->
                        latex 50 12 char AlignCentre
                , case appState of
                    Building (MousingOverTransitionLabel tId) ->
                        if tId == charID then
                            group
                                [ editIcon |> move ( 10, 0 )
                                , rect 50 20
                                    |> filled blank
                                    |> notifyTap (BMsg <| SelectTransitionLabel charID)
                                ]

                        else
                            group []

                    _ ->
                        group []
                ]
                |> move ( 0, 7 )
                |> move (p ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 ) 0.5)
                |> move
                    ( -offset * sin theta
                    , offset * cos theta
                    )
                |> notifyEnter (BMsg <| MouseOverTransitionLabel charID)
                |> notifyLeave (BMsg MouseLeaveLabel)
            ]

        {- ( (x2 + x0)
               / 2
               + rx
               / 2
               + if rx > 0 then
                   10 + 0.08 * rx
                 else
                   -10 - 0.08 * rx
           , (y2 + y0)
               / 2
               + ry
               / 2
           )
        -}
        , if sel then
            group
                [ line ( xx0, yy0 ) ( mx, my ) |> outlined (dotted 1) black
                , line ( xx1, yy1 ) ( mx, my ) |> outlined (dotted 1) black
                , circle 3
                    |> filled red
                    |> move ( mx, my )
                    |> notifyMouseDown (BMsg <| StartDraggingArrow ( s1, charID, s2 ))
                    |> notifyMouseMoveAt (BMsg << Drag)
                ]

          else
            group []
        ]


renderArrows : Set StateID -> Delta -> StatePositions -> StateTransitions -> Model -> Shape Msg
renderArrows states del pos transPos model =
    let
        oldMachine = model.machine

        stateList =
            Set.toList states

        edgeToList state =
            Dict.toList
                (case Dict.get state del of
                    Just d ->
                        d

                    Nothing ->
                        Dict.empty
                )

        getPos state =
            case Dict.get state pos of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )

        getTransPos ( s1, char, s2 ) =
            case Dict.get ( s1, char, s2 ) transPos of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )
    in
    group <|
        List.map
            (\s1 ->
                group
                    (List.concat
                        (List.map
                            (\( chId, ss ) ->
                                List.map
                                    (\s2 ->
                                        let
                                            ( x0, y0 ) =
                                                getPos s1

                                            ( x1, y1 ) =
                                                getTransPos ( s1, chId, s2 )

                                            ( x2, y2 ) =
                                                getPos s2

                                            ch =
                                                case Dict.get chId oldMachine.transitionNames of
                                                    Just c ->
                                                        c

                                                    _ ->
                                                        ""

                                            sel =
                                                case model.appState of
                                                    Building (SelectedArrow ( ss1, char, ss2 )) ->
                                                        char == chId

                                                    Building (DraggingArrow ( ss1, char, ss2 )) ->
                                                        char == chId

                                                    _ ->
                                                        False
                                        in
                                        group
                                            [ renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) 20 20 ch chId sel s1 s2 model.appState
                                            ]
                                    )
                                    [ ss ]
                            )
                            (edgeToList s1)
                        )
                    )
            )
            stateList


vertex ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        p0 =
            ( x0, y0 )

        p1 =
            ( x1, y1 )

        p2 =
            ( x2, y2 )

        p3 =
            add p0 p2

        t =
            dot (sub p0 p1) (sub p3 (mult p1 2)) / (dot p3 p3 - 4 * dot p1 (sub p3 p1))
    in
    p p0 p1 p2 t


p p0 p1 p2 t =
    add (mult p0 ((1 - t) ^ 2)) (add (mult (mult (mult p1 t) (1 - t)) 2) (mult p2 (t ^ 2)))


add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


mult ( x, y ) s =
    ( x * s, y * s )


sub ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


dot ( x0, y0 ) ( x1, y1 ) =
    x0 * x1 + y0 * y1



--List.map (\state -> )


view model =
    let
        {- accepted =
           isAccept model.states oldMachine.final model.input model.inputAt
        -}
        winX =
            toFloat <| first model.windowSize

        winY =
            toFloat <| second model.windowSize
        
        oldMachine = model.machine
    in
    collage
        winX
        --winX
        winY
        --winY
        [ case model.appState of
            Building _ ->
                rect winX winY
                    |> filled blank
                    |> (if model.holdingShift then
                            notifyTapAt (BMsg << AddState)

                        else
                            notifyTap (GoTo <| Building Regular)
                       )

            _ ->
                group []
        , group
            [ renderStates oldMachine.q model.states oldMachine.final oldMachine.statePositions model
            , renderArrows oldMachine.q oldMachine.delta oldMachine.statePositions oldMachine.stateTransitions model
            ]
            |> move
                ( 0
                , case model.appState of
                    Simulating _ ->
                        winY / 6

                    _ ->
                        0
                )

        --, renderTape model.input model.inputAt |> move ( 0, 0 )
        {- , text ("Accepted: " ++ toString accepted)
           |> centered
           |> filled
               (if accepted then
                   green
                else
                   red
               )
           |> move ( 0, -60 )
        -}
        , case model.appState of
            Building (AddingArrow s ( x, y )) ->
                let
                    s0Pos =
                        case Dict.get s oldMachine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    newTrans =
                        case List.head <| Dict.values oldMachine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys oldMachine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0
                in
                renderArrow s0Pos ( 0, 0 ) ( x, y ) 20 0 newTrans newTransID False s -1 model.appState

            Building (AddingArrowOverOtherState s ( x, y ) s1) ->
                let
                    s0Pos =
                        case Dict.get s oldMachine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    s1Pos =
                        case Dict.get s1 oldMachine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    newTrans =
                        case List.head <| Dict.values oldMachine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys oldMachine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0
                in
                renderArrow s0Pos ( 0, 0 ) s1Pos 20 20 newTrans newTransID False s -1 model.appState

            _ ->
                group []

        --, group [ roundedRect 30 30 10 |> filled lightGreen, triangle 10 |> filled white ] |> move ( 0, -100 ) |> notifyTap Step
        -- , text (Debug.toString model.appState) |> filled black |> move ( 0, -150 )
        , let
            newRect =
                rect winX winY
                    |> filled blank
                    |> notifyMouseMoveAt (BMsg << Drag)
                    |> notifyMouseUp (BMsg StopDragging)
          in
          case model.appState of
            Building (DraggingState _ _) ->
                newRect

            Building (AddingArrow _ _) ->
                newRect

            Building (AddingArrowOverOtherState _ _ _) ->
                newRect

            Building (DraggingArrow _) ->
                newRect

            _ ->
                group []

        {- , editingButtons
           |> move (-winX/2,winY/2)
        -}
        , modeButtons model
        , renderSimulate model
        ]


renderSimulate : Model -> Shape Msg
renderSimulate model =
    let
        oldMachine = model.machine
        winX =
            toFloat <| first model.windowSize

        winY =
            toFloat <| second model.windowSize

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
                        |> notifyTap (SMsg AddNewTape)
                    ]
                    |> move ( -winX / 2 + 20, winY / 6 - 35 - 25 * (toFloat <| Dict.size model.simulateData.tapes) )
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
                , case model.appState of
                    Simulating (SimRegular tapeId charId) ->
                        group (List.indexedMap (\x ( chId, ch ) -> renderTape ch chId tapeId charId True |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList tapes)
                            |> move ( -winX / 2 + 20, winY / 6 - 40 )

                    _ ->
                        group []
                ]

        tapes =
            model.simulateData.tapes
    in
    group
        [ case model.appState of
            Simulating (SimRegular _ _) ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , menu
                    ]
                    |> move ( 0, -winY / 3 )

            Simulating (SimEditing tapeId) ->
                let
                    tape =
                        case Dict.get tapeId model.simulateData.tapes of
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

            _ ->
                group []
        ]


modeButtons model =
    let
        winX =
            toFloat <| first model.windowSize

        winY =
            toFloat <| second model.windowSize

        building =
            case model.appState of
                Building _ ->
                    True

                _ ->
                    False

        simulating =
            case model.appState of
                Simulating _ ->
                    True

                _ ->
                    False
    in
    group
        [ group
            [ roundedRect 40 15 1
                |> filled
                    (if building then
                        rgb 21 137 255

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "Build"
                |> centered
                |> fixedwidth
                |> filled
                    (if building then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 25, winY / 2 - 15 )
            |> notifyTap (GoTo <| Building Regular)
        , group
            [ roundedRect 60 15 1
                |> filled
                    (if simulating then
                        rgb 21 137 255

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "Simulate"
                |> centered
                |> fixedwidth
                |> filled
                    (if simulating then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 77, winY / 2 - 15 )
            |> notifyTap (GoTo <| Simulating <| SimRegular 0 -1)
        ]


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
