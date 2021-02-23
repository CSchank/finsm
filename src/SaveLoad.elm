module SaveLoad exposing (..)

import ApplicationModel exposing (ApplicationModel)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Modal as Modal
import Bootstrap.Spinner as Spinner
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser.Events
import Dict exposing (Dict)
import Duration
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (editIcon)
import Html exposing (Html)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Machine exposing (Machine)
import Ports
import Simulating exposing (InputTape)
import Time exposing (Posix)
import Utils exposing (newMsg, textBox)


type MachineType
    = DFA
    | NFA
    | NPDA
    | Turing


type FilterType
    = FilterActive
    | MachineFilter MachineType
    | FilterArchived


filterToString : FilterType -> String
filterToString f =
    case f of
        FilterActive ->
            "all"

        MachineFilter m ->
            machineTypeStr m

        FilterArchived ->
            "arc"


decodeMachineType : D.Decoder MachineType
decodeMachineType =
    D.string
        |> D.andThen
            (\m ->
                case m of
                    "D" ->
                        D.succeed DFA

                    "N" ->
                        D.succeed NFA

                    "P" ->
                        D.succeed NPDA

                    "T" ->
                        D.succeed Turing

                    s ->
                        D.fail <| "Invalid string " ++ s ++ " for machine type"
            )


encodeMachineType : MachineType -> E.Value
encodeMachineType =
    E.string << machineTypeStr


machineTypeStr : MachineType -> String
machineTypeStr m =
    case m of
        DFA ->
            "D"

        NFA ->
            "N"

        NPDA ->
            "P"

        Turing ->
            "T"


machineTypeFullStr : MachineType -> String
machineTypeFullStr m =
    case m of
        DFA ->
            "DFA"

        NFA ->
            "NFA"

        NPDA ->
            "NPDA"

        Turing ->
            "Turing"


type alias LoadMetadata =
    { id : String
    , name : String
    , date : Posix
    , description : String
    , machine_type : MachineType
    }


decodeMetadataV1 : D.Decoder LoadMetadata
decodeMetadataV1 =
    D.map5 LoadMetadata
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "date" <| D.map Time.millisToPosix D.int)
        (D.field "desc" D.string)
        (D.field "type" decodeMachineType)


decodeMetadata : D.Decoder LoadMetadata
decodeMetadata =
    D.field "v" D.int
        |> D.andThen
            (\v ->
                case v of
                    1 ->
                        decodeMetadataV1

                    _ ->
                        D.fail <| "Invalid save metadata version " ++ String.fromInt v
            )


decodeMachineList : D.Decoder (List LoadMetadata)
decodeMachineList =
    D.list decodeMetadata


encodeMachinePayload =
    encodeMachinePayloadV1



-- encode the payload when saving a machine to the server
-- note: id is empty if the machine is a new one instead of one already saved to the server
-- sending an existing id will overwrite the machine saved with that id


encodeMachinePayloadV1 : String -> String -> Machine -> String -> Dict Int ( InputTape, a ) -> MachineType -> E.Value
encodeMachinePayloadV1 name desc machine uuid inputTape machine_type =
    E.object
        [ ( "name", E.string name )
        , ( "desc", E.string desc )
        , ( "machine", Machine.machineEncoder machine )
        , ( "v", E.int 1 )
        , ( "uuid", E.string uuid )
        , ( "tape", Simulating.inputTapeEncoder inputTape )
        , ( "type", encodeMachineType machine_type )
        ]


type alias SaveResponse =
    { success : Bool
    , uuid : String
    }


decodeSaveResponse : D.Decoder SaveResponse
decodeSaveResponse =
    D.map2 SaveResponse
        (D.field "success" D.bool)
        (D.field "uuid" D.string)


saveMachine : String -> String -> Machine -> String -> Dict Int ( InputTape, a ) -> MachineType -> (Result Http.Error SaveResponse -> msg) -> Cmd msg
saveMachine name desc machine uuid inputTape machine_type toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/save"
            (Http.jsonBody <| encodeMachinePayload name desc machine uuid inputTape machine_type)
            decodeSaveResponse


type alias ArchivePayload =
    { uuid : String
    , restore : Bool
    }


encodeArchivePayload : ArchivePayload -> E.Value
encodeArchivePayload ap =
    E.object
        [ ( "uuid", E.string ap.uuid )
        , ( "restore", E.bool ap.restore )
        ]


archiveMachine : ArchivePayload -> (Result Http.Error ArchiveResponse -> msg) -> Cmd msg
archiveMachine payload toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/archive"
            (Http.jsonBody <| encodeArchivePayload payload)
            decodeArchiveResponse


type alias LoadPayload =
    { machine : Machine
    , tapes : Dict Int InputTape
    , name : String
    , uuid : String
    }


type alias ArchiveResponse =
    { success : Bool
    }


decodeArchiveResponse : D.Decoder ArchiveResponse
decodeArchiveResponse =
    D.map ArchiveResponse (D.field "success" <| D.bool)


decodeLoadPayload : D.Decoder LoadPayload
decodeLoadPayload =
    D.map4 LoadPayload
        (D.field "machine" Machine.machineDecoder)
        (D.field "tape" Simulating.inputTapeDictDecoder)
        (D.field "name" D.string)
        (D.field "uuid" D.string)


loadMachine : String -> (Result Http.Error LoadPayload -> msg) -> Cmd msg
loadMachine uuid toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/load"
            (Http.jsonBody <| E.string uuid)
            decodeLoadPayload


loadList : FilterType -> (Result Http.Error (List LoadMetadata) -> msg) -> Cmd msg
loadList machineType toMsg =
    Http.send toMsg <|
        Http.post
            "/api/machine/list"
            (Http.stringBody "text/plain" <| filterToString machineType)
            decodeMachineList


type Msg
    = OpenLoginDialog
    | OpenLogoutDialog
    | MachineCreatedMsg MachineCreatedMsg
    | GetLoginStatus
    | ArchiveMachine String
    | RestoreMachine String
    | LoginStatusChange (Result Http.Error LoginStatus)
    | InitLoginStatus (Result Http.Error LoginStatus)
    | LoadMachine LoadMetadata
    | LoadMachineResponse (Result Http.Error LoadPayload)
    | ArchiveMachineResponse (Result Http.Error ArchiveResponse)
    | SelectFilter FilterType
    | OpenLoadDialog
    | OpenNewDialog
    | CloseLoadDialog
    | ListLoadResponse (Result Http.Error (List LoadMetadata))
    | ModalAnimation Modal.Visibility
    | CreateNewMachine



-- messages that can only be sent when there is a machine loaded


type MachineCreatedMsg
    = EditMachineName
    | TypeName String
    | SaveMachine
    | MachineSaveResponse (Result Http.Error SaveResponse)
    | AutoSave Posix
    | TabMsg Tab.State


loginStatusDecoder : D.Decoder LoginStatus
loginStatusDecoder =
    D.field "loggedin" D.bool
        |> D.andThen
            (\loggedIn ->
                if loggedIn then
                    D.map2 LoggedIn
                        (D.field "email" D.string)
                        (D.map
                            (\s ->
                                if s == "" then
                                    Nothing

                                else
                                    Just s
                            )
                         <|
                            D.field "newestMachine" D.string
                        )

                else
                    D.succeed NotLoggedIn
            )


getInitLoginStatus : Cmd Msg
getInitLoginStatus =
    Http.send InitLoginStatus <|
        Http.post
            "/accounts/loginstate/"
            Http.emptyBody
            loginStatusDecoder


getLoginStatus : Cmd Msg
getLoginStatus =
    Http.send LoginStatusChange <|
        Http.post
            "/accounts/loginstate/"
            Http.emptyBody
            loginStatusDecoder


type LoginStatus
    = LoggedIn String {- username -} (Maybe String) {- latest machine -}
    | NotLoggedIn
    | LoggingIn


initSaveModel =
    ( { loginState = NotLoggedIn
      , machineData = MachineCreated
      , loadDialog = NothingOpen
      , loadDialogModal = Modal.shown
      , machineMetadata = initMachineMetadata
      , tabState = Tab.initialState
      , loadingList = Nothing
      , editingName = False
      , lastSaved = Time.millisToPosix 0
      , unsavedChanges = False
      , loadFilter = FilterActive
      }
    , Cmd.batch [ getInitLoginStatus ]
    )


initMachineMetadata =
    { id = "", name = "Untitled", description = "", date = Time.millisToPosix 0, machine_type = DFA }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ Browser.Events.onVisibilityChange (\_ -> GetLoginStatus)
        , Ports.loginComplete (\_ -> GetLoginStatus)
        , Ports.logoutComplete (\_ -> GetLoginStatus)
        , Modal.subscriptions model.loadDialogModal ModalAnimation
        ]
            ++ (case ( model.machineData, model.loginState ) of
                    ( MachineCreated, LoggedIn _ _ ) ->
                        if model.unsavedChanges then
                            [ Time.every 5000 (MachineCreatedMsg << AutoSave) ]

                        else
                            []

                    ( _, _ ) ->
                        []
               )


type alias Model =
    { loginState : LoginStatus
    , tabState : Tab.State
    , machineData : MachineCreated
    , loadDialog : DialogStatus
    , machineMetadata : LoadMetadata
    , loadDialogModal : Modal.Visibility
    , loadingList : Maybe FilterType
    , editingName : Bool
    , lastSaved : Time.Posix
    , unsavedChanges : Bool
    , loadFilter : FilterType
    }


type MachineCreated
    = MachineCreated
    | MachineNotCreated


type DialogStatus
    = NothingOpen
    | LoadLoading
    | LoadOpen (List LoadMetadata)
    | NewOpen


type SaveStatus
    = NotSaved
    | LastSaved Time.Posix
    | Saved Time.Posix


update : Msg -> Model -> Environment -> ApplicationModel -> ( Model, Cmd Msg )
update msg model env appModel =
    case msg of
        OpenLoadDialog ->
            ( { model | loadDialog = LoadLoading }
            , Cmd.batch
                [ loadList FilterActive ListLoadResponse
                , if model.unsavedChanges then
                    newMsg (MachineCreatedMsg SaveMachine)

                  else
                    Cmd.none
                ]
            )

        ListLoadResponse response ->
            case response of
                Ok machineList ->
                    ( { model | loadDialog = LoadOpen machineList, loadDialogModal = Modal.shown, loadingList = Nothing, machineData = MachineCreated }, Cmd.none )

                Err _ ->
                    ( { model | loadDialog = NothingOpen }, Cmd.none )

        LoadMachine meta ->
            ( { model | machineMetadata = meta, loadDialog = NothingOpen }
            , loadMachine meta.id LoadMachineResponse
            )

        -- handled by Main.elm
        LoadMachineResponse _ ->
            ( model, Cmd.none )

        SelectFilter filter_type ->
            ( { model
                | tabState = Tab.customInitialState (filterToString filter_type)
                , loadingList = Just filter_type
                , loadFilter = filter_type
                , loadDialog = LoadOpen []
              }
            , loadList filter_type ListLoadResponse
            )

        OpenLoginDialog ->
            ( { model | loginState = LoggingIn }, Ports.launchLogin () )

        OpenLogoutDialog ->
            ( model, Ports.launchLogout () )

        GetLoginStatus ->
            ( model, getLoginStatus )

        LoginStatusChange loginStatus ->
            case loginStatus of
                Ok loginState ->
                    ( { model | loginState = loginState }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        InitLoginStatus loginStatus ->
            case loginStatus of
                Ok loginState ->
                    ( { model
                        | loginState = loginState
                        , loadDialog =
                            case loginState of
                                LoggedIn email latestMachine ->
                                    NothingOpen

                                NotLoggedIn ->
                                    NewOpen

                                LoggingIn ->
                                    NothingOpen
                        , loadDialogModal = Modal.shown
                      }
                    , case loginState of
                        LoggedIn _ (Just uuid) ->
                            loadMachine uuid LoadMachineResponse

                        _ ->
                            Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ArchiveMachine uuid ->
            ( model
            , archiveMachine { uuid = uuid, restore = False } ArchiveMachineResponse
            )

        RestoreMachine uuid ->
            ( model
            , archiveMachine { uuid = uuid, restore = True } ArchiveMachineResponse
            )

        ArchiveMachineResponse archiveResponse ->
            ( model, loadList model.loadFilter ListLoadResponse )

        MachineCreatedMsg mcMsg ->
            case model.machineData of
                MachineCreated ->
                    let
                        ( newModel, mcCmd ) =
                            machineCreatedUpdate env appModel mcMsg model
                    in
                    ( newModel, Cmd.map MachineCreatedMsg mcCmd )

                _ ->
                    ( model, Cmd.none )

        CloseLoadDialog ->
            ( { model | loadDialogModal = Modal.hidden, loadDialog = NothingOpen }, Cmd.none )

        ModalAnimation v ->
            ( { model | loadDialogModal = v }, Cmd.none )

        OpenNewDialog ->
            ( { model | loadDialog = NewOpen, loadDialogModal = Modal.shown }, Cmd.none )

        -- handled in Main.elm
        CreateNewMachine ->
            ( model, Cmd.none )


machineCreatedUpdate : Environment -> ApplicationModel -> MachineCreatedMsg -> Model -> ( Model, Cmd MachineCreatedMsg )
machineCreatedUpdate env appModel msg model =
    case msg of
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
                appModel.sharedModel.machine
                model.machineMetadata.id
                appModel.simulatingData.tapes
                model.machineMetadata.machine_type
                MachineSaveResponse
            )

        MachineSaveResponse saveresp ->
            let
                meta =
                    model.machineMetadata
            in
            case saveresp of
                Ok oksaveresp ->
                    ( { model
                        | machineMetadata = { meta | id = oksaveresp.uuid }
                        , lastSaved = env.currentTime
                        , unsavedChanges = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        AutoSave time ->
            ( model
            , if model.unsavedChanges then
                saveMachine
                    model.machineMetadata.name
                    model.machineMetadata.description
                    appModel.sharedModel.machine
                    model.machineMetadata.id
                    appModel.simulatingData.tapes
                    model.machineMetadata.machine_type
                    MachineSaveResponse

              else
                Cmd.none
            )

        TabMsg state ->
            ( { model | tabState = state }, Cmd.none )


view : Model -> Environment -> Shape Msg
view model env =
    let
        winX =
            toFloat <| Tuple.first env.windowSize

        winY =
            toFloat <| Tuple.second env.windowSize
    in
    group
        [ case model.loginState of
            NotLoggedIn ->
                group
                    [ roundedRect 50 15 1
                        |> filled blank
                        |> addOutline (solid 1) darkGray
                    , text "Log in"
                        |> centered
                        |> fixedwidth
                        |> filled black
                        |> move ( 0, -4 )
                    ]
                    |> move ( winX / 2 - 50, winY / 2 - 15 )
                    |> notifyTap OpenLoginDialog

            LoggedIn email lastMachine ->
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
        , case model.loadDialog of
            LoadOpen metas ->
                let
                    tab : FilterType -> Tab.Item Msg
                    tab ft =
                        Tab.item
                            { id =
                                case ft of
                                    FilterActive ->
                                        "all"

                                    MachineFilter DFA ->
                                        "D"

                                    MachineFilter NFA ->
                                        "N"

                                    MachineFilter NPDA ->
                                        "P"

                                    MachineFilter Turing ->
                                        "T"

                                    FilterArchived ->
                                        "arc"
                            , link =
                                Tab.link [ Html.Events.onClick <| SelectFilter ft ] <|
                                    []
                                        {- <|
                                           (if model.loadingList == Just ft then
                                               [ Spinner.spinner
                                                   [ Spinner.small, Spinner.attrs [ Spacing.mr1 ] ]
                                                   []
                                               ]

                                            else
                                               []
                                           )
                                        -}
                                        ++ [ Html.text
                                                (case ft of
                                                    FilterActive ->
                                                        "All"

                                                    MachineFilter DFA ->
                                                        "DFA/NFA"

                                                    MachineFilter NFA ->
                                                        "DFA/NFA"

                                                    MachineFilter NPDA ->
                                                        "NPDA"

                                                    MachineFilter Turing ->
                                                        "TM"

                                                    FilterArchived ->
                                                        "Archived"
                                                )
                                           ]
                            , pane =
                                Tab.pane []
                                    []
                            }
                in
                GraphicSVG.html winX
                    winY
                    (Modal.config CloseLoadDialog
                        -- Configure the modal to use animations providing the new AnimateModal msg
                        |> Modal.withAnimation ModalAnimation
                        |> Modal.header []
                            [ Html.div [] [ Html.h3 [] [ Html.text "My Machines" ] ]
                            , Html.div [] [ Button.button [ Button.primary, Button.attrs [ style "margin-left" "10px" ], Button.onClick OpenNewDialog ] [ Html.text "New" ] ]
                            ]
                        -- |> Modal.header [] [ Html.h3 [] [Html.text "Your Machines"] , Html.div [style "display" "block", style "float" "right"] [Button.button [Button.primary, Button.small ] [ Html.text "New" ] ] ]
                        |> Modal.body [ style "height" (String.fromFloat (winY / 2) ++ "px"), style "overflow" "scroll" ] [ renderLoadList (model.loadingList /= Nothing) (model.loadFilter == FilterArchived) metas env.currentTime env.timeZone ]
                        {- |> Modal.footer []
                           [ Button.button
                               [ Button.outlinePrimary
                               -- If you want the custom close button to use animations;
                               -- you should use the AnimateModal msg and provide it with the Modal.hiddenAnimated visibility
                               , Button.attrs [ Html.Events.onClick <| ModalAnimation Modal.hiddenAnimated ]
                               ]
                               [ Html.text "Close" ]
                           ]
                        -}
                        |> Modal.footer []
                            [ Html.div [ style "width" "100%" ]
                                [ Tab.config (MachineCreatedMsg << TabMsg)
                                    |> Tab.pills
                                    -- |> Tab.attrs [style "float" "left"]
                                    |> Tab.center
                                    |> Tab.items
                                        (List.map tab
                                            [ FilterActive
                                            , MachineFilter DFA

                                            {- , MachineFilter NPDA, MachineFilter Turing, -}
                                            , FilterArchived
                                            ]
                                        )
                                    |> Tab.view model.tabState
                                ]
                            ]
                        |> Modal.view model.loadDialogModal
                    )
                    |> move ( -winX / 2, winY / 2 )

            NewOpen ->
                GraphicSVG.html winX
                    winY
                    (Modal.config CloseLoadDialog
                        -- Configure the modal to use animations providing the new AnimateModal msg
                        |> Modal.withAnimation ModalAnimation
                        |> Modal.header []
                            [ Html.div [] [ Html.h3 [] [ Html.text "Welcome to finsm.io!" ] ]
                            ]
                        -- |> Modal.header [] [ Html.h3 [] [Html.text "Your Machines"] , Html.div [style "display" "block", style "float" "right"] [Button.button [Button.primary, Button.small ] [ Html.text "New" ] ] ]
                        |> Modal.body [ style "height" (String.fromFloat (winY / 2) ++ "px"), style "overflow" "scroll" ]
                            [ Html.h4 [] [ Html.text "finsm.io lets you create, test and export finite state machines. Get started by selecting an option below:" ]
                            , renderNew model.loginState
                            ]
                        {- |> Modal.footer []
                           [ Button.button
                               [ Button.outlinePrimary
                               -- If you want the custom close button to use animations;
                               -- you should use the AnimateModal msg and provide it with the Modal.hiddenAnimated visibility
                               , Button.attrs [ Html.Events.onClick <| ModalAnimation Modal.hiddenAnimated ]
                               ]
                               [ Html.text "Close" ]
                           ]
                        -}
                        |> Modal.footer [] []
                        |> Modal.view model.loadDialogModal
                    )
                    |> move ( -winX / 2, winY / 2 )

            _ ->
                group []
        , case model.machineData of
            MachineCreated ->
                group
                    [ if not model.editingName then
                        group
                            [ group
                                [ roundedRect 15 15 2 |> filled white |> addOutline (solid 1) darkGray |> move ( 3, 3 )
                                , editIcon
                                    |> scale 1.5
                                ]
                                |> move ( -winX / 2 + 470, winY / 2 - 20 )
                            , text model.machineMetadata.name
                                |> fixedwidth
                                |> size 16
                                |> filled black
                                |> move ( -winX / 2 + 175, winY / 2 - 20 )
                            ]
                            |> notifyTap (MachineCreatedMsg EditMachineName)

                      else
                        textBox model.machineMetadata.name 300 20 "Machine Name" (MachineCreatedMsg << TypeName)
                            |> move ( -winX / 2 + 325, winY / 2 - 10 )
                    , text (lastSaved model env)
                        |> fixedwidth
                        |> size 14
                        |> filled darkGray
                        |> move ( -winX / 2 + 490, winY / 2 - 20 )
                    ]

            MachineNotCreated ->
                group []
        ]


lastSaved : Model -> Environment -> String
lastSaved model env =
    let
        duration =
            Duration.from model.lastSaved env.currentTime
    in
    if not model.unsavedChanges then
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
        case model.loginState of
            LoggedIn _ _ ->
                "saving..."

            NotLoggedIn ->
                "log in to save changes"

            _ ->
                ""


aboutAXAgo : Duration.Duration -> String
aboutAXAgo duration =
    if Duration.inSeconds duration <= 30 then
        "just now"

    else if Duration.inSeconds duration <= 90 then
        "about a minute ago"

    else if Duration.inMinutes duration <= 60 then
        String.fromInt (round <| Duration.inMinutes duration) ++ " minutes ago"

    else if Duration.inMinutes duration <= 90 then
        "about an hour ago"

    else if Duration.inDays duration <= 1 then
        String.fromInt (round <| Duration.inHours duration) ++ " hours ago"

    else
        String.fromInt (round <| Duration.inDays duration) ++ " days ago"


dateFormat : Time.Zone -> Posix -> Posix -> String
dateFormat zn now thn =
    let
        duration =
            Duration.from thn now

        dayStr day =
            case day of
                Time.Mon ->
                    "Monday"

                Time.Tue ->
                    "Tuesday"

                Time.Wed ->
                    "Wednesday"

                Time.Thu ->
                    "Thursday"

                Time.Fri ->
                    "Friday"

                Time.Sat ->
                    "Saturday"

                Time.Sun ->
                    "Sunday"

        monStr mon =
            case mon of
                Time.Jan ->
                    "January"

                Time.Feb ->
                    "February"

                Time.Mar ->
                    "March"

                Time.Apr ->
                    "April"

                Time.May ->
                    "May"

                Time.Jun ->
                    "June"

                Time.Jul ->
                    "July"

                Time.Aug ->
                    "August"

                Time.Sep ->
                    "September"

                Time.Oct ->
                    "October"

                Time.Nov ->
                    "November"

                Time.Dec ->
                    "December"

        dateFmt : Posix -> String
        dateFmt t =
            (monStr <| Time.toMonth zn t) ++ " " ++ (String.fromInt <| Time.toDay zn t) ++ ", " ++ (String.fromInt <| Time.toYear zn t)
    in
    if Duration.inDays duration <= 1 then
        aboutAXAgo duration

    else if Duration.inDays duration <= 3 then
        dayStr (Time.toWeekday zn thn)

    else
        dateFmt thn


renderLoadList : Bool -> Bool -> List LoadMetadata -> Posix -> Time.Zone -> Html Msg
renderLoadList loadingList archiveList metas now zn =
    let
        oneRow machine =
            ListGroup.anchor
                [ ListGroup.attrs [ Flex.col, Flex.alignItemsStart, Size.w100 ]
                ]
                [ Html.div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ Html.h5 [ Spacing.mb1 ] [ Html.text machine.name ]
                    , Html.small [] [ Html.text <| dateFormat zn now machine.date ]
                    ]
                , ButtonGroup.buttonGroup [ ButtonGroup.attrs [ style "float" "right" ] ]
                    [ ButtonGroup.button [ Button.primary, Button.small, Button.onClick (LoadMachine machine) ] [ Html.text "Open" ]
                    , ButtonGroup.button
                        [ Button.danger
                        , Button.small
                        , Button.onClick
                            (if archiveList then
                                RestoreMachine machine.id

                             else
                                ArchiveMachine machine.id
                            )
                        ]
                        [ Html.text
                            (if archiveList then
                                "Restore"

                             else
                                "Archive"
                            )
                        ]
                    ]
                , Html.div [] [ Html.b [] [ Html.text (machineTypeFullStr machine.machine_type) ] ]
                ]
    in
    Html.div []
        --[style "overflow" "scroll"]-- style "width" (String.fromInt w ++ "px"), style "height" (String.fromInt h ++ "px"), style "position" "fixed"]
        [ if loadingList then
            Html.div [ style "height" "500px" ] [ Spinner.spinner [ Spinner.color Text.primary, Spinner.large, Spinner.grow, Spinner.attrs [ style "display" "block", style "margin" "auto" ] ] [] ]

          else if metas == [] then
            Html.div [ style "text-align" "center" ] [ Html.text "No machines matching current filter." ]

          else
            ListGroup.custom (List.map oneRow metas)
        ]


renderNew : LoginStatus -> Html Msg
renderNew loginStatus =
    Card.deck
        [ Card.config []
            |> Card.headerH3 [] [ Html.text "DFA / NFA" ]
            |> Card.block []
                [ Block.text [] [ Html.text "Create a new Finite State Machine." ] ]
            |> Card.footer []
                [ Button.button [ Button.primary, Button.onClick CreateNewMachine ] [ Html.text "Create!" ] ]
        , case loginStatus of
            LoggedIn _ _ ->
                Card.config []
                    |> Card.headerH3 [] [ Html.text "Load Existing" ]
                    |> Card.block []
                        [ Block.text [] [ Html.text "Load an existing machine." ] ]
                    |> Card.footer []
                        [ Button.button [ Button.primary, Button.onClick OpenLoadDialog ] [ Html.text "Load" ] ]

            NotLoggedIn ->
                Card.config []
                    |> Card.headerH3 [] [ Html.text "Load Existing" ]
                    |> Card.block []
                        [ Block.text [] [ Html.text "Log in to load an existing machine." ] ]
                    |> Card.footer []
                        [ Button.button [ Button.primary, Button.onClick OpenLoginDialog ] [ Html.text "Login" ] ]

            LoggingIn ->
                Card.config []
                    |> Card.headerH3 [] [ Html.text "Load Existing" ]
                    |> Card.block []
                        [ Block.text [] [ Html.text "Please finish logging in to load your machines." ] ]
                    |> Card.footer []
                        [ Button.button [ Button.primary, Button.onClick OpenLoginDialog ] [ Html.text "Login" ] ]
        , Card.config []
            |> Card.headerH3 [] [ Html.text "Get involved!" ]
            |> Card.block []
                [ Block.text [] [ Html.text "Check us out on GitHub." ] ]
            |> Card.footer []
                [ Button.linkButton [ Button.primary, Button.attrs [ Html.Attributes.href "https://github.com/cschank/finsm" ] ] [ Html.text "Go!" ] ]
        ]
