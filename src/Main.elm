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
import Machine exposing (..)
import SharedModel exposing (SharedModel)
import Environment exposing(Environment)
import Building
import Simulating
import Helpers
import Url exposing (Url)


type Msg
    = BMsg Building.Msg
    | SMsg Simulating.Msg
    | KeyPressed Int
    | KeyReleased Int
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest


type ApplicationState
    = Building Building.Model
    | Simulating Simulating.Model


type alias Model =
    { appState : ApplicationState
    , simulateModel : Simulating.PersistentModel
    , buildingModel : Building.PersistentModel
    , sharedModel : SharedModel
    , environment : Environment
    }


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appState = Building Building.init
                  , sharedModel = SharedModel.init
                  , simulateData = Simulating.init
                  , environment = Environment.init
                  }
                , Task.perform (\vp -> WindowSize ( round vp.viewport.width, round vp.viewport.height )) Browser.Dom.getViewport
                )
        , update = update
        , view = \m -> { body = view m, title = "finsm - create and simulate finite state machines" }
        , subscriptions =
            \model ->
                Sub.batch
                    [ Browser.Events.onResize (\w h -> WindowSize ( w, h ))
                    , Browser.Events.onKeyDown (D.map KeyPressed (D.field "keyCode" D.int))
                    , Browser.Events.onKeyUp (D.map KeyReleased (D.field "keyCode" D.int))
                    , case model.appState of
                        Building m -> Sub.map BMsg (Simulating.subscriptions m)
                        Simulating m -> Sub.map SMsg (Simulating.subscriptions m)
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




--renameState : State -> String ->



isAccept : Set StateID -> Set StateID -> InputTape -> Int -> Bool
isAccept states finals input inputAt =
    if inputAt == Array.length input then
        Set.size (Set.intersect states finals) > 0

    else
        False


textHtml : String -> Html msg
textHtml t =
    H.span
        [ Json.Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


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
        [ 
          modeButtons model
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