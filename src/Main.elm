module Main exposing (..)

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
import Machine exposing (..)
import SharedModel exposing (SharedModel)
import Environment
import Building
import Simulating
import Helpers

type Msg
    = GoTo ApplicationState
    | BMsg Building.Msg
    | SMsg Simulating.Msg
    | KeyPressed Int
    | KeyReleased Int
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest


type SimulatingMsg
    = Step
    | EditTape Int
    | DeleteTape Int
    | AddNewTape
    | ChangeTape Int
    | ToggleStart StateID







type ApplicationState
    = Building Building.Model
    | Simulating Simulating.Model






{- source StateID -}


type SimulatingState
    = SimRegular Int {- tapeID -} Int {- charID -}
    | SimEditing Int



{- tapeID -}







type alias Model =
    { appState : ApplicationState
    , simulateModel : Simulating.PersistentModel
    , buildingModel : Building.PersistentModel
    , sharedModel : SharedModel
    , environment : Environment
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


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appState = Building Building.Regular
                  , sharedModel = SharedModel.init
                  , simulateData = initSimData
                  , environment = Environment.init
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
        oldMachine =
            model.machine

        oldEnvironment =
            model.environment
    in
    case msg of
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
            ( { model | environment = { oldEnvironment | windowSize = ( w, h ) } }, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        KeyReleased k ->
            if k == 16 then
                ( { model | environment = { oldEnvironment | holdingShift = False } }, Cmd.none )

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
                                | machine = { oldMachine | transitionNames = Dict.insert tId newLbl oldMachine.transitionNames }
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
                                { oldMachine
                                    | delta = newDelta
                                    , stateTransitions = newStateTransitions
                                    , transitionNames = Dict.remove tId oldMachine.transitionNames
                                }

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
                            oldSimulateData =
                                model.simulateData
                        in
                        ( { model
                            | simulateData =
                                { oldSimulateData
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case m of
                                                    Just ar ->
                                                        Just <| Array.slice 0 -1 ar

                                                    _ ->
                                                        m
                                            )
                                            oldSimulateData.tapes
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
                ( { model | environment = { oldEnvironment | holdingShift = True } }, Cmd.none )

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

                            oldSimulateData =
                                model.simulateData
                        in
                        ( { model
                            | simulateData =
                                { oldSimulateData
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
                                            oldSimulateData.tapes
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


simulatingUpdate : SimulatingMsg -> Model -> ( Model, Cmd Msg )
simulatingUpdate msg model =
    let
        oldMachine =
            model.machine

        oldSimulateData =
            model.simulateData
    in
    case msg of
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
                            | simulateData = { oldSimulateData | currentStates = deltaHat oldMachine.transitionNames oldMachine.delta nextCh oldSimulateData.currentStates }
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
            ( { model | simulateData = { oldSimulateData | tapes = Dict.remove tId oldSimulateData.tapes } }, Cmd.none )

        AddNewTape ->
            let
                newId =
                    (case List.maximum <| Dict.keys model.simulateData.tapes of
                        Just n ->
                            n

                        Nothing ->
                            0
                    )
                        + 1
            in
            ( { model | simulateData = { oldSimulateData | tapes = Dict.insert newId Array.empty oldSimulateData.tapes } }, Cmd.none )

        ChangeTape tId ->
            ( { model
                | simulateData = { oldSimulateData | currentStates = oldMachine.start }
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
                        , simulateData = { oldSimulateData | currentStates = newMachine.start }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


setMax : Set Int -> Int
setMax s =
    Set.foldl max 0 s



--renameState : State -> String ->



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

textHtml : String -> Html msg
textHtml t =
    H.span
        [ Json.Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


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

view model =
    let
        {- accepted =
           isAccept model.states oldMachine.final model.input model.inputAt
        -}
        winX =
            toFloat <| first model.environment.windowSize

        winY =
            toFloat <| second model.environment.windowSize

        oldMachine =
            model.machine
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
                    |> (if model.environment.holdingShift then
                            notifyTapAt (BMsg << AddState)

                        else
                            notifyTap (GoTo <| Building Regular)
                       )

            _ ->
                group []
        , 
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




modeButtons model =
    let
        winX =
            toFloat <| first model.environment.windowSize

        winY =
            toFloat <| second model.environment.windowSize

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