module Simulating exposing (HoverError, InputTape, Model(..), Msg(..), PersistentModel, TapeStatus(..), checkTape, checkTapes, delta, deltaHat, epsTrans, initPModel, isAccept, latexKeyboard, machineDefn, onEnter, onExit, renderTape, subscriptions, update, view)

import Array exposing (Array)
import Browser.Events
import Debug exposing (todo)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Error exposing (..)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Json.Decode as D
import Machine exposing (..)
import Mistakes exposing (..)
import Set exposing (Set)
import SharedModel exposing (..)
import Task
import Tuple exposing (first, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown (D.map KeyPressed (D.field "key" D.string))


type alias PersistentModel =
    { tapes : Dict Int ( InputTape, TapeStatus )
    }


type alias InputTape =
    Array Character


type TapeStatus
    = Fresh
    | Stale (Set String)


type TapeSimStatus
    = InProgress
    | Dead
    | Accept


type AcceptType
    = FinalState
    | EmptyStack


type alias HoverError =
    Maybe Int


type alias Stack =
    List Character


type alias Configuration =
    { readHead : Int
    , state : StateID
    , stack : Stack
    , tapeSimStatus : TapeSimStatus
    }


type Model
    = Default HoverError
    | Editing Int
    | Simulating Int {- tapeID of what I'm simulating currently -} (List Configuration) -- Stacks. Should this be a dict instead?


type Msg
    = Step
    | EditTape Int
    | DeleteTape Int
    | AddNewTape
    | ToggleStart StateID
    | KeyPressed String
    | MachineMsg Machine.Msg
    | HoverErrorEnter Int
    | HoverErrorExit
    | SimulateTape Int


onEnter : Environment -> ( PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
onEnter env ( pModel, sModel ) =
    ( ( Default Nothing
      , { pModel
            | tapes = checkTapes sModel pModel.tapes
        }
      , sModel
      )
    , False
    , Cmd.none
    )


onExit : Environment -> ( Model, PersistentModel, SharedModel ) -> ( ( PersistentModel, SharedModel ), Bool )
onExit env ( model, pModel, sModel ) =
    ( ( pModel, sModel ), False )


initPModel : PersistentModel
initPModel =
    { tapes =
        Dict.fromList
            [ ( 0, ( Array.fromList [ "[", "[", "]", "]" ], Fresh ) )
            , ( 1, ( Array.fromList [ "[", "[", "]" ], Fresh ) )
            ]
    }


checkTapes : SharedModel -> Dict Int ( InputTape, TapeStatus ) -> Dict Int ( InputTape, TapeStatus )
checkTapes sModel tapes =
    Dict.map (\k ( tape, _ ) -> ( tape, checkTape sModel tape )) tapes


checkTape : SharedModel -> InputTape -> TapeStatus
checkTape sModel inp =
    let
        tNames =
            sModel.machine.transitionNames

        allTransitionLabels =
            Set.fromList <| List.map tripFst <| Dict.values tNames

        arrFilter =
            Array.filter (\v -> not <| Set.member v allTransitionLabels) inp
    in
    case Array.isEmpty arrFilter of
        True ->
            Fresh

        False ->
            Stale <| Set.fromList <| Array.toList arrFilter


renderTape : Model -> Array Character -> TapeStatus -> Int -> Maybe Int {- -> Int -> -} -> Shape Msg
renderTape model input tapeSt tapeId mReadHead =
    {- selectedId inputAt -}
    let
        showButtons =
            case model of
                Default _ ->
                    True

                _ ->
                    False

        hoverOn =
            case model of
                Default (Just errId) ->
                    if errId == tapeId then
                        True

                    else
                        False

                _ ->
                    False

        xpad =
            20

        errWindow =
            group
                [ roundedRect 800 30 2
                    |> filled white
                    |> addOutline (solid 1) darkGray
                    |> move ( 400, 5 )
                , text "This tape has stale transitions. Modify or delete it!"
                    |> size 25
                    |> fixedwidth
                    |> filled red
                ]
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
                                (if tapeSt == Fresh then
                                    black

                                 else
                                    red
                                )
                            |> move ( 0, 3 )
                        , latex (xpad * 0.9) (xpad * 0.7) "white" st AlignCentre
                            |> move ( 0, 10.25 )
                        ]
                        |> move
                            ( toFloat n
                                * xpad
                                + (if not showButtons then
                                    xpad / 2

                                   else
                                    0
                                  )
                            , 0
                            )
                )
                input
            )
            ++ (case mReadHead of
                    Just inputAt ->
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
                       |> move ( xpad + xpad * toFloat inputAt, 0 )
                       ]
                    _ ->
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
                        |> notifyTap (EditTape tapeId)
                    , group
                        [ roundedRect 15 15 2
                            |> filled white
                            |> addOutline (solid 1) darkGray
                        , trashIcon |> scale 0.2 |> move ( -2, 0 )
                        ]
                        |> move ( toFloat <| (Array.length input + 1) * xpad, 3 )
                        |> notifyTap (DeleteTape tapeId)
                    , group
                        [ roundedRect 15 15 2
                            |> filled white
                            |> addOutline (solid 1) darkGray
                        , simulateIcon |> scale 0.4 |> move ( -2, 0 )
                        ]
                        |> move ( toFloat <| (Array.length input + 2) * xpad, 3 )
                        |> notifyTap (SimulateTape tapeId)
                    , if not (tapeSt == Fresh) then
                        group
                            ([ triangle 20 |> filled red |> rotate 22.5
                             , roundedRect 7.5 10 5 |> filled white |> move ( 0, 7.5 )
                             , circle 3 |> filled white |> move ( 0, -2.5 )
                             ]
                                ++ (if hoverOn then
                                        [ errWindow ]

                                    else
                                        []
                                   )
                            )
                            |> scale 0.5
                            |> move ( toFloat <| (Array.length input + 3) * xpad, 1 )
                            |> notifyEnter (HoverErrorEnter tapeId)
                            |> notifyLeave HoverErrorExit

                      else
                        group []
                    ]

                else
                    []
               )



-- need to know past stack state to be able to highlight new stack changes


renderStack : ( StateID, Stack ) -> Shape Msg
renderStack ( stId, stk ) =
    let
        singleShape stkChar =
            group
                [ rect 15 10
                    |> outlined (solid 1) black
                , latex 0 0 "white" stkChar AlignCentre

                -- |> move ( 0, 10.25 )
                ]
    in
    group <| List.indexedMap (\idx ch -> singleShape ch |> move ( 0, toFloat idx * 10 )) stk


renderConfig : Model -> SharedModel -> InputTape -> Int -> Configuration -> Shape Msg
renderConfig model sModel inpTape tapeID cfg =
    let
        m =
            sModel.machine

        stk =
            renderStack ( cfg.state, cfg.stack ) |> move (60, 0) -- placeholder

        stateShape =
            group
                [ circle 20
                    |> filled blank
                    |> addOutline (solid 1) black
                , group
                    [ latex 25 18 "none" (Maybe.withDefault "D:" <| Dict.get cfg.state m.stateNames) AlignCentre
                        |> move ( 0, 9 )
                    ]
                ]

        renderedInpTape =
            renderTape model inpTape Fresh tapeID (Just cfg.readHead)
    in
    group
        [ stk
        , stateShape
        , renderedInpTape |> move ( 25, 0 )
        ]


update : Environment -> Msg -> ( Model, PersistentModel, SharedModel ) -> ( ( Model, PersistentModel, SharedModel ), Bool, Cmd Msg )
update env msg ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine
    in
    case msg of
        -- deltaHat : SharedModel -> InputTape -> TransitionNames -> Delta -> List Configuration -> List Configuration
        Step ->
            case model of
                Simulating tapeId configs ->
                    let
                        -- filter dead/accepted configs here
                        fConfigs =
                            List.filter (\cfg -> cfg.tapeSimStatus == InProgress) configs
                                
                        inpTape =
                            Maybe.withDefault ( Array.fromList [], Fresh ) (Dict.get tapeId pModel.tapes)
                                |> Tuple.first

                        nextConfigs =
                            Debug.log  "nextCfgs" <| deltaHat sModel inpTape oldMachine.transitionNames oldMachine.delta fConfigs
                    in
                    if True then
                        ( ( Simulating tapeId nextConfigs
                          , pModel
                          , sModel
                          )
                        , False
                        , Cmd.none
                        )

                    else
                        ( ( model, pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        EditTape tId ->
            ( ( Editing tId, pModel, sModel ), False, Cmd.none )

        SimulateTape tId ->
            let
                mTapeTuple =
                    Dict.get tId pModel.tapes

                isFresh =
                    case mTapeTuple of
                        Just ( _, Fresh ) ->
                            True

                        _ ->
                            False

                initCfg : Configuration
                initCfg =
                    { readHead = -1
                    , stack = [ "\\bot"]
                    , state = oldMachine.start
                    , tapeSimStatus = InProgress
                    }
            in
            if isFresh then
                ( ( Simulating tId [ initCfg ], pModel, sModel ), False, Cmd.none )

            else
                ( ( model, pModel, sModel ), False, Cmd.none )

        DeleteTape tId ->
            let
                newModel =
                    case model of
                        _ ->
                            model
            in
            ( ( model, { pModel | tapes = Dict.remove tId pModel.tapes }, sModel ), True, Cmd.none )

        AddNewTape ->
            let
                newId =
                    (case List.maximum <| Dict.keys pModel.tapes of
                        Just n ->
                            n

                        Nothing ->
                            0
                    )
                        + 1
            in
            ( ( model, { pModel | tapes = Dict.insert newId ( Array.empty, Fresh ) pModel.tapes }, sModel ), True, Cmd.none )

        KeyPressed k ->
            if k == "Enter" then
                case model of
                    Editing tId ->
                        ( ( Default Nothing, pModel, sModel ), True, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "Escape" then
                case model of
                    Simulating _ _ ->
                        ( ( Default Nothing, pModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "Backspace" || k == "ArrowLeft" then
                case model of
                    Editing tapeId ->
                        let
                            newPModel =
                                { pModel
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case m of
                                                    Just ( ar, tapeSt ) ->
                                                        let
                                                            newTape =
                                                                Array.slice 0 -1 ar

                                                            freshSt =
                                                                checkTape sModel newTape
                                                        in
                                                        Just ( Array.slice 0 -1 ar, freshSt )

                                                    _ ->
                                                        m
                                            )
                                            pModel.tapes
                                }
                        in
                        ( ( model, newPModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "ArrowRight" then
                case model of
                    Simulating _ _ ->
                        ( ( model, pModel, sModel ), False, Task.perform identity (Task.succeed <| Step) )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else if k == "ArrowLeft" then
                case model of
                    Simulating tId _ ->
                        -- currentStates = sModel.machine.start
                        ( ( Simulating tId [], pModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

            else
                case model of
                    Editing tapeId ->
                        let
                            charCode =
                                case k of
                                    "a" ->
                                        0

                                    "s" ->
                                        1

                                    "d" ->
                                        2

                                    "f" ->
                                        3

                                    "g" ->
                                        4

                                    "h" ->
                                        5

                                    "j" ->
                                        6

                                    "k" ->
                                        7

                                    "l" ->
                                        8

                                    "q" ->
                                        9

                                    "w" ->
                                        10

                                    "e" ->
                                        11

                                    "r" ->
                                        12

                                    "t" ->
                                        13

                                    "y" ->
                                        14

                                    "u" ->
                                        15

                                    "i" ->
                                        16

                                    "o" ->
                                        17

                                    "p" ->
                                        18

                                    "z" ->
                                        19

                                    "x" ->
                                        20

                                    "c" ->
                                        21

                                    "v" ->
                                        22

                                    "b" ->
                                        23

                                    "n" ->
                                        24

                                    "m" ->
                                        25

                                    _ ->
                                        -1

                            chars =
                                Array.fromList <| Set.toList <| Set.remove "\\epsilon" <| Set.fromList <| List.map tripFst <| Dict.values oldMachine.transitionNames

                            newChar =
                                Array.get charCode chars

                            newPModel =
                                { pModel
                                    | tapes =
                                        Dict.update tapeId
                                            (\m ->
                                                case ( m, newChar ) of
                                                    ( Just ( ar, tapeSt ), Just ch ) ->
                                                        Just ( Array.push ch ar, tapeSt )

                                                    ( Nothing, Just ch ) ->
                                                        Just ( Array.fromList [ ch ], Fresh )

                                                    _ ->
                                                        m
                                            )
                                            pModel.tapes
                                }
                        in
                        ( ( model, newPModel, sModel ), False, Cmd.none )

                    _ ->
                        ( ( model, pModel, sModel ), False, Cmd.none )

        MachineMsg mmsg ->
            case mmsg of
                StartDragging sId _ ->
                    ( ( model, pModel, sModel ), False, sendMsg (ToggleStart sId) )

                TapState sId ->
                    ( ( model, pModel, sModel ), False, sendMsg (ToggleStart sId) )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        ToggleStart sId ->
            let
                tests =
                    oldMachine.start

                newMachine =
                    { oldMachine
                        | start = sId
                    }
            in
            case model of
                Default hErr ->
                    ( ( Default hErr, pModel, { sModel | machine = newMachine } ), True, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        HoverErrorEnter tapeId ->
            case model of
                Default _ ->
                    ( ( Default (Just tapeId), pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )

        HoverErrorExit ->
            case model of
                Default _ ->
                    ( ( Default Nothing, pModel, sModel ), False, Cmd.none )

                _ ->
                    ( ( model, pModel, sModel ), False, Cmd.none )


isAccept : Set StateID -> Set StateID -> InputTape -> Int -> Bool
isAccept states finals input inputAt =
    if inputAt == Array.length input then
        Set.size (Set.intersect states finals) > 0

    else
        False


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        oldMachine =
            sModel.machine

        winX =
            toFloat <| first env.windowSize

        winY =
            toFloat <| second env.windowSize

        transMistakes =
            Nothing

        -- getTransitionMistakes sModel.machine
        chars =
            -- This is broken?
            Set.toList <| Set.remove "\\epsilon" <| Set.fromList <| List.map tripFst <| Dict.values oldMachine.transitionNames

        defaultMenu =
            group <|
                [ text "Simulate"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 2, winY / 6 - 15 )
                , group
                    [ roundedRect 15 15 2
                        |> filled white
                        |> addOutline (solid 1) darkGray
                    , text "+"
                        |> size 16
                        |> fixedwidth
                        |> filled black
                        |> move ( -4.5, -5 )
                        |> notifyTap AddNewTape
                    ]
                    |> move ( -winX / 2 + 20, winY / 6 - 35 - 25 * (toFloat <| Dict.size pModel.tapes) )
                , case model of
                    Default _ ->
                        group (List.indexedMap (\x ( chId, ( ch, tapeSt ) ) -> renderTape model ch tapeSt chId Nothing |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList pModel.tapes)
                            |> move ( -winX / 2 + 20, winY / 6 - 40 )

                    _ ->
                        group []
                ]

        simMenu =
            group <|
                [ text "Simulate"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 2, winY / 6 - 15 )
                , text "Press ESC to exit, right arrow to scrub through tape"
                    |> size 6
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 85, winY / 6 - 15 )

                {- , case model of
                   Simulating tId configs ->
                       let
                           tapeInfo =
                               Dict.get tId pModel.tapes
                       in
                       case tapeInfo of
                           Nothing ->
                               group []

                           -- Error!
                           Just ( tapeInp, tapeSt ) ->
                               renderTape model tapeInp tapeSt tId |> move ( -winX / 2 + 20, winY / 6 - 40 )

                   _ ->
                       group []
                -}
                ]

        {-
           validCheck =
               machineCheck sModel
        -}
    in
    group
        [ case model of
            Default _ ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , machineDefn sModel winX winY
                    , defaultMenu
                    ]
                    |> move ( 0, -winY / 3 )

            Editing tapeId ->
                let
                    ( tape, tapeSt ) =
                        case Dict.get tapeId pModel.tapes of
                            Just ( t, st ) ->
                                ( t, st )

                            Nothing ->
                                ( Array.empty, Fresh )
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
                    , renderTape model tape tapeSt tapeId Nothing
                        |> move ( -10 * toFloat (Array.length tape), winY / 6 - 65 )
                    ]
                    |> move ( 0, -winY / 3 )

            Simulating tapeId cfgs ->
                -- PLACEHOLDER
                let
                    inpTape =
                        case Dict.get tapeId pModel.tapes of
                            Just ( tape, _ ) ->
                                tape

                            Nothing ->
                                Array.empty
                in
                group
                    ([ rect winX (winY / 3)
                        |> filled lightGray
                     , simMenu
                     ]
                        ++ (List.indexedMap (\idx cfg -> renderConfig model sModel inpTape tapeId cfg |> move ( toFloat idx * 20 - winX/3 , 0 )) cfgs) 
                    )
                    |> move ( 0, -winY / 3 )
        , (GraphicSVG.map MachineMsg <| Machine.view env Regular sModel.machine (Set.singleton sModel.machine.start)) |> move ( 0, winY / 6 )
        ]


machineDefn : SharedModel -> Float -> Float -> Shape Msg
machineDefn sModel winX winY =
    let
        machine =
            sModel.machine

        getStateName sId =
            case Dict.get sId machine.stateNames of
                Just n ->
                    n

                Nothing ->
                    "\\ "

        machineHeader =
            text "Machine"
                |> size 16
                |> fixedwidth
                |> filled black
                |> move ( -winX / 2 + 492, winY / 6 - 15 )
    in
    group
        [ machineHeader
        , latex 500 18 "blank" "let\\ M = (Q,\\Sigma,\\Gamma,\\delta,s,\\bot,F)" AlignLeft
            |> move ( -winX / 2 + 500, winY / 6 - 25 )
        , latex 500 14 "blank" "where" AlignLeft
            |> move ( -winX / 2 + 500, winY / 6 - 45 )
        , latex 500 18 "blank" ("Q = \\{ " ++ String.join "," (Dict.values machine.stateNames) ++ " \\}") AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 65 )
        , latex 500 18 "blank" ("\\Sigma = \\{ " ++ String.join "," (Set.toList <| Set.remove "\\epsilon" <| Set.fromList <| List.map tripFst <| Dict.values machine.transitionNames) ++ " \\}") AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 90 )
        , latex 500 18 "blank" ("\\Gamma = \\{ " ++ String.join "," (Dict.values machine.transitionNames |> List.map tripSnd |> Set.fromList |> Set.toList) ++ " \\}") AlignLeft
            -- LMD: Making assumption that stack alphabets are completely derivable from the current stack symbol value
            |> move ( -winX / 2 + 510, winY / 6 - 115 )
        , latex 500 18 "blank" "\\delta = (above)" AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 140 )
        , latex 500 18 "blank" "\\bot = \\bot" AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 165 )
        , latex 500
            14
            "blank"
            ("s = "
                ++ (if machine.start == -1 then
                        "Please\\ select\\ a\\ start\\ state"

                    else
                        getStateName machine.start
                   )
            )
            AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 190 )
        , latex 500 18 "blank" ("F = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| machine.final) ++ " \\}") AlignLeft
            |> move ( -winX / 2 + 510, winY / 6 - 215 )
        ]


epsTrans : TransitionNames -> Delta -> StateID -> List Configuration
epsTrans tNames d states =
    todo "todo"



{-
   let
       dList =
           (Dict.toList << Dict.filter (\k _ -> Set.member k states)) d

       -- LMD: This was copy-pasted from delta
       getName trans =
           case Dict.get trans tNames of
               Just ( n, _, _ ) ->
                   n

               _ ->
                   ""

       findEpsTransitions : List ( StateID, Dict TransitionID StateID ) -> List StateID
       findEpsTransitions lst =
           case lst of
               [] ->
                   []

               ( sID, dictTrans ) :: xs ->
                   let
                       listTrans =
                           Dict.toList dictTrans

                       epsStates =
                           List.filterMap
                               (\( tId, sId ) ->
                                   if getName tId == "\\epsilon" then
                                       Just sId

                                   else
                                       Nothing
                               )
                               listTrans
                   in
                   epsStates ++ findEpsTransitions xs

       newCurrentStates =
           Set.union (Set.fromList <| findEpsTransitions dList) states
   in
   if newCurrentStates == states then
       states

   else
       epsTrans tNames d newCurrentStates

-}


delta : SharedModel -> InputTape -> TransitionNames -> Delta -> Configuration -> List Configuration
delta sModel inpTape tNames d cfg =
    let
        getName transID =
            case Dict.get transID tNames of
                Just transition ->
                    transition

                _ ->
                    ( "", "", "" )

        -- error string?
        ch =
            case Array.get (cfg.readHead+1) inpTape of
                Just char ->
                    char

                Nothing ->
                    ""

        predStackTop curTop =
            -- need to push this error (stack top >1 char) into Build mode. Put here for now
            case List.head cfg.stack of
                Just char -> -- Debug.log "pred" <|
                    case String.toList curTop of
                        c :: [] ->
                            char == String.fromList (c :: [])

                        _ ->
                            False

                Nothing ->
                    False
    in
    case Dict.get cfg.state d of
        Just transMap ->
            List.map
                (\( tId, sId ) ->
                    let
                        isEpsTrans = (tripFst (getName tId) == "\\epsilon" && sId == cfg.state)
                        pushNothing = tripThd (getName tId) == "\\epsilon"
                    in
                    if
                        (ch == tripFst (getName tId) && (predStackTop <| tripSnd <| getName tId))
                    then
                        { readHead = cfg.readHead + 1
                        , state = sId
                        , stack = if pushNothing then cfg.stack else String.split "" (tripThd (getName tId)) ++ cfg.stack
                        , tapeSimStatus =
                            if isEpsTrans && (cfg.readHead + 1) == Array.length inpTape - 1
                                    && Set.member sId sModel.machine.final
                                -- This is acceptance by final state, need to program both in!
                            then
                                Accept

                            else
                                InProgress
                        }
                    else if isEpsTrans
                    then
                        { readHead = cfg.readHead
                        , state = sId
                        , stack = if pushNothing then cfg.stack else String.split "" (tripThd (getName tId)) ++ cfg.stack
                        , tapeSimStatus = InProgress
                        }
                    else
                        { cfg | tapeSimStatus = Dead }
                )
            <|
                Dict.toList transMap

        Nothing ->
            [ { cfg | tapeSimStatus = Dead } ]



-- deltaHat : TransitionNames -> Delta -> Character -> Set StateID -> Set StateID


deltaHat : SharedModel -> InputTape -> TransitionNames -> Delta -> List Configuration -> List Configuration
deltaHat sModel inpTape tNames d cfgs =
    List.concat (List.map (delta sModel inpTape tNames d) cfgs)


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
                , latex (keyW / 1.5) (keyH / 1.5) "white" char AlignCentre
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
