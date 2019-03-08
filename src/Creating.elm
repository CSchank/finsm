module Creating exposing (Model(..), Msg(..), PersistentModel, exportButton, init, initPModel, onEnter, onExit, subscriptions, update, view)

import Convert exposing (..)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Kleene exposing (Kleene(..))
import Machine exposing (textBox)
import ParserKleene exposing (parseKleene)
import SharedModel exposing (..)
import Tuple exposing (first, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias PersistentModel =
    String


type Model
    = Regular
    | Confirmation
    | ParseError


type Msg
    = SendRegex
    | Input String


init =
    Regular


initPModel =
    ""


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Regular, pModel, sModel ), False, Cmd.none )


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
        SendRegex ->
            let
                kleeneExpr =
                    parseKleene pModel
            in
            case kleeneExpr of
                Empty ->
                    ( ( ParseError, pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( Confirmation, pModel, { sModel | machine = convertNFA kleeneExpr, machineType = NFA } ), False, Cmd.none )

        Input s ->
            ( ( Regular, s, sModel ), False, Cmd.none )


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        winX =
            toFloat <| first env.windowSize

        winY =
            toFloat <| second env.windowSize
    in
    group
        [ text "Enter your regular expression:"
            |> size 20
            |> fixedwidth
            |> filled black
            |> move ( -winX / 8, winY / 20 )
        , textBox pModel 200 20 "Enter your Regex" Input
        , exportButton
            |> move ( 0, -winY / 20 - 10 )
            |> notifyTap SendRegex
        , case model of
            Regular ->
                group []

            Confirmation ->
                text "Machine generated! :)"
                    |> size 12
                    |> fixedwidth
                    |> filled darkGreen
                    |> move ( -winX / 20, -winY / 10 - 5 )

            ParseError ->
                text "Parse error, or memory limit exceeded! :("
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( -winX / 10, -winY / 10 - 5 )
        ]


exportButton =
    group
        [ roundedRect 130 40 5
            |> filled finsmBlue
        , text "Generate"
            |> fixedwidth
            |> size 24
            |> centered
            |> filled white
            |> move ( 0, -7 )
        ]
