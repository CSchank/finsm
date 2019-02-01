module Main exposing (ApplicationModel, ApplicationState(..), Model, Module(..), Msg(..), initAppModel, main, modeButtons, textHtml, update, view)

import Array exposing (Array)
import BetterUndoList exposing (..)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events
import Building
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode
import List
import Machine exposing (..)
import Random
import Set exposing (Set)
import SharedModel exposing (SharedModel)
import Simulating
import Task
import Tuple exposing (first, second)
import Url exposing (Url)
import Debug


type Msg
    = BMsg Building.Msg
    | SMsg Simulating.Msg
    | KeyPressed Int
    | KeyReleased Int
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest
    | GoTo Module


type Module
    = BuildingModule
    | SimulatingModule


type ApplicationState
    = Building Building.Model
    | Simulating Simulating.Model


type alias Model =
    { appModel : BetterUndoList ApplicationModel
    , environment : Environment
    }


type alias ApplicationModel =
    { appState : ApplicationState
    , simulatingData : Simulating.PersistentModel
    , buildingData : Building.PersistentModel
    , sharedModel : SharedModel
    }


initAppModel : BetterUndoList ApplicationModel
initAppModel =
    fresh
        { appState = Building Building.init
        , sharedModel = SharedModel.init
        , simulatingData = Simulating.initPModel
        , buildingData = Building.initPModel
        }


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appModel = initAppModel
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
                    , case model.appModel.present.appState of
                        Building m ->
                            Sub.map BMsg (Building.subscriptions m)

                        Simulating m ->
                            Sub.map SMsg (Simulating.subscriptions m)
                    ]
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }



{- replace : state -> UndoList state -> UndoList state
   replace st stul =
       { stul | present = st }
-}


update msg model =
    let
        oldEnvironment =
            model.environment

        currentAppState =
            model.appModel.present
    in
    case msg of
        BMsg bmsg ->
            case currentAppState.appState of
                Building m ->
                    let
                        ( ( newM, newPModel, newSModel ), checkpoint, cmd ) =
                            Building.update oldEnvironment (Tuple.first <| Debug.log "bmsg" (bmsg,m)) ( m, currentAppState.buildingData, currentAppState.sharedModel )

                        newAppState =
                            { currentAppState
                                | appState = Building newM
                                , buildingData = newPModel
                                , sharedModel = newSModel
                            }
                    in
                    ( { model
                        | appModel =
                            if checkpoint then
                                new newAppState model.appModel

                            else
                                replace newAppState model.appModel
                      }
                    , Cmd.map BMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        SMsg smsg ->
            case currentAppState.appState of
                Simulating m ->
                    let
                        ( ( newM, newPModel, newSModel ), checkpoint, cmd ) =
                            Simulating.update oldEnvironment smsg ( m, currentAppState.simulatingData, currentAppState.sharedModel )

                        newAppState =
                            { currentAppState
                                | appState = Simulating newM
                                , simulatingData = newPModel
                                , sharedModel = newSModel
                            }
                    in
                    ( { model
                        | appModel =
                            if checkpoint then
                                new newAppState model.appModel

                            else
                                replace newAppState model.appModel
                      }
                    , Cmd.map SMsg cmd
                    )

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

            else if k == 91 then
                ( { model | environment = { oldEnvironment | holdingMeta = False } }, Cmd.none )

            else if k == 17 then
                ( { model | environment = { oldEnvironment | holdingControl = False } }, Cmd.none )

            else
                ( model, Cmd.none )

        KeyPressed k ->
            if k == 16 then
                ( { model | environment = { oldEnvironment | holdingShift = True } }, Cmd.none )

            else if k == 89 {- y -} || k == 90 {- z -} then
                let
                    doUndo =
                        (oldEnvironment.holdingControl || oldEnvironment.holdingMeta) && k == 90

                    doRedo =
                        (oldEnvironment.holdingControl && k == 89)
                            || (oldEnvironment.holdingMeta && oldEnvironment.holdingShift && k == 90)
                in
                ( { model
                    | appModel =
                        if doRedo then
                            redo model.appModel

                        else if doUndo then
                            undo model.appModel

                        else
                            model.appModel
                  }
                , Cmd.none
                )

            else if k == 91 then
                --pressed meta key
                ( { model | environment = { oldEnvironment | holdingMeta = True } }, Cmd.none )

            else if k == 17 then
                --pressed control
                ( { model | environment = { oldEnvironment | holdingControl = True } }, Cmd.none )

            else
                ( model, Cmd.none )

        GoTo mod ->
            let
                exit =
                    case currentAppState.appState of
                        Building m ->
                            let
                                ( ( pModel, sModel ), checkpoint ) =
                                    Building.onExit oldEnvironment ( m, currentAppState.buildingData, currentAppState.sharedModel )

                                newAppState =
                                    { currentAppState | buildingData = pModel, sharedModel = sModel }
                            in
                            if checkpoint then
                                new newAppState model.appModel

                            else
                                replace newAppState model.appModel

                        Simulating m ->
                            let
                                ( ( pModel, sModel ), checkpoint ) =
                                    Simulating.onExit oldEnvironment ( m, currentAppState.simulatingData, currentAppState.sharedModel )

                                newAppState =
                                    { currentAppState | simulatingData = pModel, sharedModel = sModel }
                            in
                            if checkpoint then
                                new newAppState model.appModel

                            else
                                replace newAppState model.appModel

                ( enter, cmd ) =
                    case mod of
                        BuildingModule ->
                            let
                                ( ( bModel, pModel, sModel ), checkpoint, bCmd ) =
                                    Building.onEnter oldEnvironment ( exit.present.buildingData, exit.present.sharedModel )

                                newAppState =
                                    { currentAppState | appState = Building bModel, buildingData = pModel, sharedModel = sModel }
                            in
                            ( if checkpoint then
                                new newAppState model.appModel

                              else
                                replace newAppState model.appModel
                            , Cmd.map BMsg bCmd
                            )

                        SimulatingModule ->
                            let
                                ( ( simModel, pModel, sModel ), checkpoint, sCmd ) =
                                    Simulating.onEnter oldEnvironment ( exit.present.simulatingData, exit.present.sharedModel )

                                newAppState =
                                    { currentAppState | appState = Simulating simModel, simulatingData = pModel, sharedModel = sModel }
                            in
                            ( if checkpoint then
                                new newAppState model.appModel

                              else
                                replace newAppState model.appModel
                            , Cmd.map SMsg sCmd
                            )
            in
            ( { model | appModel = enter }, cmd )


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

        appState =
            model.appModel.present
    in
    collage
        winX
        --winX
        winY
        --winY
        [ case appState.appState of
            Building m ->
                GraphicSVG.map BMsg <| Building.view model.environment ( m, appState.buildingData, appState.sharedModel )

            Simulating m ->
                GraphicSVG.map SMsg <| Simulating.view model.environment ( m, appState.simulatingData, appState.sharedModel )
        , modeButtons model
        ]


modeButtons model =
    let
        winX =
            toFloat <| first model.environment.windowSize

        winY =
            toFloat <| second model.environment.windowSize

        building =
            case model.appModel.present.appState of
                Building _ ->
                    True

                _ ->
                    False

        simulating =
            case model.appModel.present.appState of
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
            |> notifyTap (GoTo BuildingModule)
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
            |> notifyTap (GoTo SimulatingModule)
        ]
