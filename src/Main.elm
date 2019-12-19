module Main exposing (ApplicationModel, ApplicationState(..), Model, Module(..), Msg(..), initAppModel, main, modeButtons, textHtml, update, view)

import Array exposing (Array)
import BetterUndoList exposing (..)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events exposing (Visibility)
import Building
import Dict exposing (Dict)
import Environment exposing (Environment)
import Exporting
import GraphicSVG exposing (..)
import Helpers exposing (finsmBlue, icon, sendMsg)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import Json.Encode
import List
import Machine exposing (..)
import Ports
import Random
import SaveLoad exposing (saveMachine)
import Set exposing (Set)
import SharedModel exposing (SharedModel)
import Simulating
import Task
import Time
import Tuple exposing (first, second)
import Url exposing (Url)
import Utils exposing (textBox)


type Msg
    = BMsg Building.Msg
    | SMsg Simulating.Msg
    | EMsg Exporting.Msg
    | KeyPressed String
    | KeyReleased String
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest
    | GoTo Module
    | VisibilityChanged Visibility
    | OpenLoginDialog
    | OpenLogoutDialog
    | GetLoginStatus
    | LoginStatusChange (Result Http.Error LoginStatus)
    | EditMachineName
    | TypeName String
    | SaveMachine
    | MachineSaveResponse (Result Http.Error SaveLoad.SaveResponse)
    | NoOp


loginStatusDecoder : D.Decoder LoginStatus
loginStatusDecoder =
    D.field "loggedin" D.bool
        |> D.andThen
            (\loggedIn ->
                if loggedIn then
                    D.map LoggedIn (D.field "email" D.string)

                else
                    D.succeed NotLoggedIn
            )


getLoginStatus : Cmd Msg
getLoginStatus =
    Http.send LoginStatusChange <|
        Http.post
            "/accounts/loginstate/"
            Http.emptyBody
            loginStatusDecoder


type Module
    = BuildingModule
    | SimulatingModule
    | ExportingModule


type ApplicationState
    = Building Building.Model
    | Simulating Simulating.Model
    | Exporting Exporting.Model


type LoginStatus
    = LoggedIn String
    | NotLoggedIn
    | LoggingIn


type alias Model =
    { appModel : BetterUndoList ApplicationModel
    , environment : Environment
    , loginState : LoginStatus
    , loginDialog : Bool
    , machineMetadata : SaveLoad.LoadMetadata
    , editingName : Bool
    }


type alias ApplicationModel =
    { appState : ApplicationState
    , simulatingData : Simulating.PersistentModel
    , buildingData : Building.PersistentModel
    , exportingData : Exporting.PersistentModel
    , sharedModel : SharedModel
    }


initAppModel : BetterUndoList ApplicationModel
initAppModel =
    fresh
        { appState = Building Building.init
        , sharedModel = SharedModel.init
        , simulatingData = Simulating.initPModel
        , buildingData = Building.initPModel
        , exportingData = Exporting.initPModel
        }


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appModel = initAppModel
                  , environment = Environment.init
                  , loginState = NotLoggedIn
                  , loginDialog = False
                  , machineMetadata = { id = "", name = "Untitled", description = "", date = Time.millisToPosix 0 }
                  , editingName = False
                  }
                , Cmd.batch
                    [ Task.perform (\vp -> WindowSize ( round vp.viewport.width, round vp.viewport.height )) Browser.Dom.getViewport
                    , getLoginStatus
                    ]
                )
        , update = update
        , view = \m -> { body = view m, title = "finsm - create and simulate finite state machines" }
        , subscriptions =
            \model ->
                Sub.batch
                    [ Browser.Events.onResize (\w h -> WindowSize ( w, h ))
                    , Browser.Events.onKeyDown (D.map KeyPressed (D.field "key" D.string))
                    , Browser.Events.onKeyUp (D.map KeyReleased (D.field "key" D.string))
                    , Browser.Events.onVisibilityChange VisibilityChanged
                    , case model.appModel.present.appState of
                        Building m ->
                            Sub.map BMsg (Building.subscriptions m)

                        Simulating m ->
                            Sub.map SMsg (Simulating.subscriptions m)

                        Exporting m ->
                            Sub.map EMsg (Exporting.subscriptions m)
                    , Browser.Events.onVisibilityChange (\_ -> GetLoginStatus)
                    , Ports.loginComplete (\_ -> GetLoginStatus)
                    , Ports.logoutComplete (\_ -> GetLoginStatus)
                    ]
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }



{- replace : state -> UndoList state -> UndoList state
   replace st stul =
       { stul | present = st }
-}


moduleUpdate :
    Environment
    -> mMsg
    -> mModel
    -> pModel
    -> Model
    -> (mMsg -> Msg)
    -> (mModel -> ApplicationState)
    -> (pModel -> ApplicationModel -> ApplicationModel)
    -> (Environment -> mMsg -> ( mModel, pModel, SharedModel ) -> ( ( mModel, pModel, SharedModel ), Bool, Cmd mMsg ))
    -> ( Model, Cmd Msg )
moduleUpdate env mMsg mModel pModel model msgWrapper appStateWrapper setpModel mUpdate =
    let
        currentAppState =
            model.appModel.present

        ( ( newM, newPModel, newSModel ), checkpoint, cmd ) =
            mUpdate env mMsg ( mModel, pModel, currentAppState.sharedModel )

        newAppState =
            { currentAppState
                | appState = appStateWrapper newM
                , sharedModel = newSModel
            }
                |> setpModel newPModel
    in
    ( { model
        | appModel =
            if checkpoint then
                new newAppState model.appModel

            else
                replace newAppState model.appModel
      }
    , Cmd.map msgWrapper cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
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
                    moduleUpdate
                        oldEnvironment
                        bmsg
                        m
                        currentAppState.buildingData
                        model
                        BMsg
                        Building
                        (\pm am -> { am | buildingData = pm })
                        Building.update

                _ ->
                    ( model, Cmd.none )

        SMsg smsg ->
            case currentAppState.appState of
                Simulating m ->
                    moduleUpdate
                        oldEnvironment
                        smsg
                        m
                        currentAppState.simulatingData
                        model
                        SMsg
                        Simulating
                        (\pm am -> { am | simulatingData = pm })
                        Simulating.update

                _ ->
                    ( model, Cmd.none )

        EMsg emsg ->
            case currentAppState.appState of
                Exporting m ->
                    moduleUpdate
                        oldEnvironment
                        emsg
                        m
                        currentAppState.exportingData
                        model
                        EMsg
                        Exporting
                        (\pm am -> { am | exportingData = pm })
                        Exporting.update

                _ ->
                    ( model, Cmd.none )

        WindowSize ( w, h ) ->
            ( { model | environment = { oldEnvironment | windowSize = ( w, h ) } }, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        KeyReleased k ->
            if k == "Shift" then
                ( { model | environment = { oldEnvironment | holdingShift = False } }, Cmd.none )

            else if k == "Meta" then
                ( { model | environment = { oldEnvironment | holdingMeta = False } }, Cmd.none )

            else if k == "Control" then
                ( { model | environment = { oldEnvironment | holdingControl = False } }, Cmd.none )

            else if k == "Enter" then
                ( { model | editingName = False }, Cmd.none )

            else
                ( model, Cmd.none )

        KeyPressed k ->
            if k == "Shift" then
                ( { model | environment = { oldEnvironment | holdingShift = True } }, Cmd.none )

            else if k == "y" || k == "z" then
                let
                    doUndo =
                        (oldEnvironment.holdingControl || oldEnvironment.holdingMeta) && k == "z"

                    doRedo =
                        (oldEnvironment.holdingControl && k == "y")
                            || (oldEnvironment.holdingMeta && oldEnvironment.holdingShift && k == "z")
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

            else if k == "Meta" then
                --pressed meta key
                ( { model | environment = { oldEnvironment | holdingMeta = True } }, Cmd.none )

            else if k == "Control" then
                --pressed control
                ( { model | environment = { oldEnvironment | holdingControl = True } }, Cmd.none )
                {- else if k == 66 then
                       ( model, sendMsg <| GoTo BuildingModule )

                   else if k == 83 then
                       ( model, sendMsg <| GoTo SimulatingModule )
                -}

            else
                ( model, Cmd.none )

        GoTo mod ->
            let
                exit =
                    case currentAppState.appState of
                        Building m ->
                            processExit
                                oldEnvironment
                                m
                                currentAppState.buildingData
                                model
                                (\pm am -> { am | buildingData = pm })
                                Building.onExit

                        Simulating m ->
                            processExit
                                oldEnvironment
                                m
                                currentAppState.simulatingData
                                model
                                (\pm am -> { am | simulatingData = pm })
                                Simulating.onExit

                        Exporting m ->
                            processExit
                                oldEnvironment
                                m
                                currentAppState.exportingData
                                model
                                (\pm am -> { am | exportingData = pm })
                                Exporting.onExit

                ( enter, cmd ) =
                    case mod of
                        BuildingModule ->
                            processEnter
                                oldEnvironment
                                currentAppState.buildingData
                                exit
                                BMsg
                                Building
                                (\pm am -> { am | buildingData = pm })
                                Building.onEnter

                        SimulatingModule ->
                            processEnter
                                oldEnvironment
                                currentAppState.simulatingData
                                exit
                                SMsg
                                Simulating
                                (\pm am -> { am | simulatingData = pm })
                                Simulating.onEnter

                        ExportingModule ->
                            processEnter
                                oldEnvironment
                                currentAppState.exportingData
                                exit
                                EMsg
                                Exporting
                                (\pm am -> { am | exportingData = pm })
                                Exporting.onEnter
            in
            ( { model | appModel = enter }, cmd )

        VisibilityChanged vis ->
            ( { model
                | environment =
                    { oldEnvironment
                        | holdingShift = False
                        , holdingControl = False
                        , holdingMeta = False
                    }
              }
            , Cmd.none
            )

        OpenLoginDialog ->
            ( { model | loginDialog = not model.loginDialog }, Ports.launchLogin () )

        OpenLogoutDialog ->
            ( { model | loginDialog = not model.loginDialog }, Ports.launchLogout () )

        GetLoginStatus ->
            ( model, getLoginStatus )

        LoginStatusChange loginStatus ->
            case loginStatus of
                Ok loginState ->
                    ( { model | loginState = loginState }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        EditMachineName ->
            ( { model | editingName = True }, Cmd.none )

        TypeName n ->
            let
                meta =
                    model.machineMetadata
            in
            ( { model | machineMetadata = { meta | name = n } }, Cmd.none )

        SaveMachine ->
            ( model
            , saveMachine
                model.machineMetadata.name
                model.machineMetadata.description
                model.appModel.present.sharedModel.machine
                model.machineMetadata.id
                model.appModel.present.simulatingData.tapes
                MachineSaveResponse
            )

        MachineSaveResponse saveresp ->
            let
                meta =
                    model.machineMetadata
            in
            case saveresp of
                Ok oksaveresp ->
                    ( { model | machineMetadata = { meta | id = oksaveresp.uuid } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


processExit :
    Environment
    -> mModel
    -> pModel
    -> Model
    -> (pModel -> ApplicationModel -> ApplicationModel)
    -> (Environment -> ( mModel, pModel, SharedModel ) -> ( ( pModel, SharedModel ), Bool ))
    -> BetterUndoList ApplicationModel
processExit env m pModel model setpModel onExit =
    let
        currentAppState =
            model.appModel.present

        ( ( newPModel, newSModel ), checkpoint ) =
            onExit env ( m, pModel, currentAppState.sharedModel )

        newAppState =
            { currentAppState | sharedModel = newSModel }
                |> setpModel newPModel
    in
    if checkpoint then
        new newAppState model.appModel

    else
        replace newAppState model.appModel


processEnter :
    Environment
    -> pModel
    -> BetterUndoList ApplicationModel
    -> (mMsg -> Msg)
    -> (mModel -> ApplicationState)
    -> (pModel -> ApplicationModel -> ApplicationModel)
    -> (Environment -> ( pModel, SharedModel ) -> ( ( mModel, pModel, SharedModel ), Bool, Cmd mMsg ))
    -> ( BetterUndoList ApplicationModel, Cmd Msg )
processEnter env pModel exitModel msgWrapper appStateWrapper setpModel onEnter =
    let
        exitAppState =
            exitModel.present

        ( ( newM, newPModel, newSModel ), checkpoint, mCmd ) =
            onEnter env ( pModel, exitAppState.sharedModel )

        newAppState =
            { exitAppState | appState = appStateWrapper newM, sharedModel = newSModel }
                |> setpModel newPModel
    in
    ( if checkpoint then
        new newAppState exitModel

      else
        replace newAppState exitModel
    , Cmd.map msgWrapper mCmd
    )


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

            Exporting m ->
                GraphicSVG.map EMsg <| Exporting.view model.environment ( m, appState.exportingData, appState.sharedModel )
        , modeButtons model
        , icon False (text "?" |> size 30 |> fixedwidth |> centered |> filled (rgb 220 220 220) |> move ( 0, -9 ))
            |> addHyperlink "https://github.com/CSchank/finsm/wiki"
            |> move ( winX / 2 - 25, -winY / 2 + 25 )
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

        exporting =
            case model.appModel.present.appState of
                Exporting _ ->
                    True

                _ ->
                    False
    in
    group
        [ group
            [ roundedRect 40 15 1
                |> filled
                    (if building then
                        finsmBlue

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
                        finsmBlue

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
        , group
            [ roundedRect 50 15 1
                |> filled
                    (if exporting then
                        finsmBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "Export"
                |> centered
                |> fixedwidth
                |> filled
                    (if exporting then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 134, winY / 2 - 15 )
            |> notifyTap (GoTo ExportingModule)
        , case model.loginState of
            NotLoggedIn ->
                group
                    [ roundedRect 50 15 1
                        |> filled
                            (if exporting then
                                finsmBlue

                             else
                                blank
                            )
                        |> addOutline (solid 1) darkGray
                    , text "Log in"
                        |> centered
                        |> fixedwidth
                        |> filled black
                        |> move ( 0, -4 )
                    ]
                    |> move ( winX / 2 - 50, winY / 2 - 15 )
                    |> notifyTap OpenLoginDialog

            LoggedIn email ->
                group
                    [ text ("Welcome " ++ email)
                        |> alignRight
                        |> fixedwidth
                        |> filled
                            black
                        |> move ( 0, -4 )
                    , group
                        [ roundedRect 55 15 1
                            |> filled
                                (if exporting then
                                    finsmBlue

                                 else
                                    blank
                                )
                            |> addOutline (solid 1) darkGray
                        , text "Log out"
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move ( 0, -4 )
                        ]
                        |> move ( 40, 0 )
                        |> notifyTap OpenLogoutDialog
                    ]
                    |> move ( winX / 2 - 100, winY / 2 - 15 )

            _ ->
                group []
        , if not model.editingName then
            text model.machineMetadata.name
                |> fixedwidth
                |> size 16
                |> filled black
                |> move ( -winX / 2 + 250, winY / 2 - 20 )
                |> notifyTap EditMachineName

          else
            textBox model.machineMetadata.name 1000 20 "Machine Name" TypeName
                |> move ( -winX / 2 + 700, winY / 2 - 20 )
        , group
            [ roundedRect 50 15 1
                |> filled
                    (if exporting then
                        finsmBlue

                     else
                        blank
                    )
                |> addOutline (solid 1) darkGray
            , text "Save"
                |> centered
                |> fixedwidth
                |> filled
                    (if exporting then
                        white

                     else
                        darkGray
                    )
                |> move ( 0, -4 )
            ]
            |> move ( -winX / 2 + 183, winY / 2 - 15 )
            |> notifyTap SaveMachine

        --, if model.loginDialog then
        ]


errorEpsTrans model =
    let
        winX =
            toFloat <| first model.environment.windowSize

        winY =
            toFloat <| second model.environment.windowSize
    in
    group
        [ rectangle winX winY
            |> filled darkGray
            |> makeTransparent 0.75
        , group
            [ roundedRect 300 150 1 |> filled lightGray
            , text "finsm: Build Error"
                |> bold
                |> centered
                |> filled lightRed
                |> scale 2
                |> move ( 0, 40 )
            , text "You have invalid states:"
                |> filled darkRed
                |> scale 1.2
                |> move ( -140, 5 )
            , text "> Maybe ε-transitions are used with other transitions?"
                |> filled darkRed
                |> move ( -140, -10 )
            , text "> Hint: Fix transitions highlighted in red"
                |> filled darkGreen
                |> move ( -140, -25 )
            , text "Hit any key to dismiss this message"
                |> bold
                |> centered
                |> filled black
                |> scale 1.25
                |> move ( 0, -60 )
            ]
        ]
