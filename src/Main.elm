module Main exposing (ApplicationModel, ApplicationState(..), Model, Module(..), Msg(..), initAppModel, main, modeButtons, textHtml, update, view)

import Array exposing (Array)
import BetterUndoList exposing (..)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events exposing (Visibility)
import Building
import Dict exposing (Dict)
import Duration
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
import Bootstrap.ListGroup as ListGroup
import Html
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Button as Button


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
    | OpenLoadDialog
    | GetLoginStatus
    | LoginStatusChange (Result Http.Error LoginStatus)
    | EditMachineName
    | TypeName String
    | SaveMachine
    | MachineSaveResponse (Result Http.Error SaveLoad.SaveResponse)
    | ListLoadResponse (Result Http.Error (List SaveLoad.LoadMetadata))
    | LoadMachine SaveLoad.LoadMetadata
    | LoadMachineResponse (Result Http.Error SaveLoad.LoadPayload)
    | GetTime Time.Posix
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


type SaveStatus
    = NotSaved
    | LastSaved Time.Posix
    | Saved Time.Posix


type alias Model =
    { appModel : BetterUndoList ApplicationModel
    , environment : Environment
    , saveModel : SaveModel
    }


type LoadDialog =
    LoadNotOpen
    | LoadLoading
    | LoadOpen (List SaveLoad.LoadMetadata)

type alias SaveModel =
    { loginState : LoginStatus
    , loadDialog : LoadDialog
    , machineMetadata : SaveLoad.LoadMetadata
    , editingName : Bool
    , lastSaved : Time.Posix
    , unsavedChanges : Bool
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
    fresh initAppRecord


initAppRecord = { appState = Building Building.init
        , sharedModel = SharedModel.init
        , simulatingData = Simulating.initPModel
        , buildingData = Building.initPModel
        , exportingData = Exporting.initPModel
        }

initSaveModel = { loginState = NotLoggedIn
                , loadDialog = LoadNotOpen
                , machineMetadata = { id = "", name = "Untitled", description = "", date = Time.millisToPosix 0 }
                , editingName = False
                , lastSaved = Time.millisToPosix 0
                , unsavedChanges = True
                }


main : App () Model Msg
main =
    app
        { init =
            \flags url key ->
                ( { appModel = initAppModel
                  , environment = Environment.init
                  , saveModel = initSaveModel
                  }
                , Cmd.batch
                    [ Task.perform (\vp -> WindowSize ( round vp.viewport.width, round vp.viewport.height )) Browser.Dom.getViewport
                    , getLoginStatus
                    , Task.perform GetTime Time.now
                    ]
                )
        , update = update
        , view = \m -> { body = view m, title = "finsm - create and simulate finite state machines" }
        , subscriptions =
            \model ->
                Sub.batch <|
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
                    , Time.every 5000 GetTime -- get the new time every 10 seconds
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
            mUpdate env (Debug.log "mMsg" mMsg) ( mModel, pModel, currentAppState.sharedModel )

        newAppState =
            { currentAppState
                | appState = appStateWrapper newM
                , sharedModel = newSModel
            }
                |> setpModel newPModel

        sm =
            model.saveModel

        cp =
            Debug.log "unsavedChanges" sm.unsavedChanges
    in
    ( { model
        | appModel =
            if checkpoint then
                new newAppState model.appModel

            else
                replace newAppState model.appModel
        , saveModel =
            { sm
                | unsavedChanges =
                    if checkpoint then
                        True

                    else
                        sm.unsavedChanges
            }
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

        sm =
            model.saveModel
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
                ( { model | saveModel = { sm | editingName = False, unsavedChanges = True } }, Cmd.none )

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
                    , saveModel = { sm | unsavedChanges = doRedo || doUndo }
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
            ( model , Ports.launchLogin () )

        OpenLogoutDialog ->
            (model , Ports.launchLogout () )

        OpenLoadDialog ->
            ( { model | saveModel = { sm | loadDialog = LoadLoading } }, SaveLoad.loadList ListLoadResponse )

        GetLoginStatus ->
            ( model, getLoginStatus )

        LoginStatusChange loginStatus ->
            case loginStatus of
                Ok loginState ->
                    ( { model | saveModel = { sm | loginState = loginState } }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        EditMachineName ->
            ( { model | saveModel = { sm | editingName = True } }, Cmd.none )

        TypeName n ->
            let
                meta =
                    model.saveModel.machineMetadata
            in
            ( { model | saveModel = { sm | machineMetadata = { meta | name = n } } }, Cmd.none )

        SaveMachine ->
            ( model
            , saveMachine
                model.saveModel.machineMetadata.name
                model.saveModel.machineMetadata.description
                model.appModel.present.sharedModel.machine
                model.saveModel.machineMetadata.id
                model.appModel.present.simulatingData.tapes
                MachineSaveResponse
            )

        MachineSaveResponse saveresp ->
            let
                meta =
                    model.saveModel.machineMetadata
            in
            case saveresp of
                Ok oksaveresp ->
                    ( { model
                        | saveModel =
                            { sm
                                | machineMetadata = { meta | id = oksaveresp.uuid }
                                , lastSaved = model.environment.currentTime
                                , unsavedChanges = False
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GetTime time ->
            let
                oldEnv =
                    model.environment
            in
            ( { model | environment = { oldEnv | currentTime = time } }
            , if sm.unsavedChanges then
                saveMachine
                    model.saveModel.machineMetadata.name
                    model.saveModel.machineMetadata.description
                    model.appModel.present.sharedModel.machine
                    model.saveModel.machineMetadata.id
                    model.appModel.present.simulatingData.tapes
                    MachineSaveResponse

              else
                Cmd.none
            )

        ListLoadResponse response ->
            case Debug.log "machineListResponse" response of
                Ok machineList ->
                    ( { model | saveModel = { sm | loadDialog = LoadOpen machineList } }, Cmd.none )
                Err _ ->
                    ( { model | saveModel = { sm | loadDialog = LoadNotOpen } }, Cmd.none )

        LoadMachine meta ->
            ( { model  | saveModel = { sm | machineMetadata = meta, loadDialog = LoadNotOpen }}
            , SaveLoad.loadMachine meta.id LoadMachineResponse)

        LoadMachineResponse response ->
            case Debug.log "loadMachineResponse" response of
                Ok loadPayload ->
                    let
                        initSharedModel = SharedModel.init
                        newSharedModel = { initSharedModel | machine = loadPayload.machine }
                        initSimModel = Simulating.initPModel

                                --{ appState = Building Building.init
                                --, sharedModel = SharedModel.init
                                --, simulatingData = Simulating.initPModel
                                --, buildingData = Building.initPModel
                                --, exportingData = Exporting.initPModel
                                --}
                        newModel =
                                fresh {
                                    initAppRecord |
                                  sharedModel = newSharedModel
                                , simulatingData = { initSimModel | tapes = Simulating.checkTapesNoStatus newSharedModel loadPayload.tapes }
                                }
                    in
                    ( { model | appModel = newModel
                      }
                    , Cmd.none)
                Err _ ->
                    (model, Cmd.none)


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
        , case model.saveModel.loginState of
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
                        |> filled black
                        |> move ( 0, -4 )
                    , group
                        [ roundedRect 55 15 1
                            |> filled blank
                            |> addOutline (solid 1) darkGray
                        , text "Log out"
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move ( 0, -4 )
                        ]
                        |> move ( 40, 0 )
                        |> notifyTap OpenLogoutDialog
                    , group
                        [ roundedRect 85 15 1
                            |> filled blank
                            |> addOutline (solid 1) darkGray
                        , text "My Machines"
                            |> centered
                            |> fixedwidth
                            |> filled black
                            |> move ( 0, -4 )
                        ]
                        |> move ( 40, -20 )
                        |> notifyTap OpenLoadDialog
                    ]
                    |> move ( winX / 2 - 100, winY / 2 - 15 )

            _ ->
                group []
        , if not model.saveModel.editingName then
            text model.saveModel.machineMetadata.name
                |> fixedwidth
                |> size 16
                |> filled black
                |> move ( -winX / 2 + 175, winY / 2 - 20 )
                |> notifyTap EditMachineName

          else
            textBox model.saveModel.machineMetadata.name 300 20 "Machine Name" TypeName
                |> move ( -winX / 2 + 325, winY / 2 - 10 )
        , text (lastSaved model)
            |> fixedwidth
            |> size 14
            |> filled darkGray
            |> move ( -winX / 2 + 490, winY / 2 - 20 )
        , case model.saveModel.loadDialog of
            LoadNotOpen -> group []
            LoadLoading -> group [text "Loading" |> fixedwidth |> centered |> size 24 |> filled black]
            LoadOpen metas ->
                let
                    (w,h) = model.environment.windowSize
                in
                GraphicSVG.html (toFloat w/2) (toFloat h/2) (renderLoadList (w//2) (h//2) metas)
                     |> move(-(toFloat w)/4,(toFloat h)/4)

             --group (List.indexedMap (\n meta -> text (meta.name) |> filled black |> move(0,10*toFloat n)) metas)
        ]


lastSaved : Model -> String
lastSaved model =
    let
        duration =
            Duration.from model.saveModel.lastSaved model.environment.currentTime
    in
    if not model.saveModel.unsavedChanges then
        if Duration.inSeconds duration <= 30 then
            "last edit saved just now"

        else if Duration.inSeconds duration <= 90 then
            "last edit saved about a minute ago"

        else if Duration.inMinutes duration <= 60 then
            "last edit saved " ++ String.fromInt (round <| Duration.inMinutes duration) ++ " minutes ago"

        else if Duration.inMinutes duration <= 90 then
            "last edit saved about an hour ago"

        else
            "last edit saved " ++ String.fromInt (round <| Duration.inHours duration) ++ " hours ago"

    else
        case model.saveModel.loginState of
            LoggedIn _ ->
                "saving..."

            NotLoggedIn ->
                "log in to save changes"

            _ ->
                ""

renderLoadList : Int -> Int -> List SaveLoad.LoadMetadata -> Html Msg
renderLoadList w h metas =
    let
        oneRow machine =
            ListGroup.anchor
                 [ ListGroup.attrs [ Html.Events.onClick (LoadMachine machine), Flex.col, Flex.alignItemsStart ]
                 ]
                 [ Html.div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                     [ Html.h5 [ Spacing.mb1 ] [ Html.text machine.name ]
                     , Html.small [] [ Html.text "3 days ago" ]
                     ]
                 , ButtonGroup.buttonGroup [ButtonGroup.attrs [style "float" "right"]]
                       [ ButtonGroup.button [ Button.primary, Button.small ] [  Html.text "Load" ]
                       , ButtonGroup.button [ Button.danger, Button.small ] [  Html.text "Archive" ]
                       ]
                 ]
    in
        Html.div [style "overflow" "scroll", style "width" (String.fromInt w ++ "px"), style "height" (String.fromInt h ++ "px"), style "position" "fixed"]
            [ Html.h3 [] [Html.text "Load a saved machine"]
            ,ListGroup.custom (List.map oneRow metas)]


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
