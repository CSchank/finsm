module MachineFA exposing (..)

import Debug exposing (todo)
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, id, placeholder, style, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import MachineCommon exposing (..)
import Set exposing (Set)
import Utils exposing (decodeDict, decodePair, decodeSet, decodeTriple, encodeDict, encodePair, encodeSet, encodeTriple, textBox)


type alias TransitionNames =
    Dict TransitionID (Set Character)


type alias MachineFA =
    { core : MachineCore
    , start : Set StateID
    , final : Set StateID
    , transitionNames : TransitionNames
    }

machineFAEncoder : MachineFA -> E.Value
machineFAEncoder =
    machineFAEncoderV1


machineFAEncoderV1 : MachineFA -> E.Value
machineFAEncoderV1 machine =
    let

        startEncoder : Set StateID -> E.Value
        startEncoder =
            encodeSet E.int

        finalEncoder : Set StateID -> E.Value
        finalEncoder =
            encodeSet E.int

        transNamesEncoder : TransitionNames -> E.Value
        transNamesEncoder =
            encodeDict E.int (encodeSet E.string)
    in
    E.object
        [ ( "core", machineCoreEncoder machine.core)
        , ( "start", startEncoder machine.start )
        , ( "final", finalEncoder machine.final )
        , ( "transNames", transNamesEncoder machine.transitionNames )
        , ( "v", E.int 1 )
        ]


machineFADecoder : D.Decoder MachineFA
machineFADecoder =
    D.field "v" D.int
        |> D.andThen
            (\v ->
                case v of
                    1 ->
                        machineFADecoderV1

                    _ ->
                        D.fail <| "Invalid save metadata version " ++ String.fromInt v
            )


machineFADecoderV1 : D.Decoder MachineFA
machineFADecoderV1 =
    let
        startDecoder : D.Decoder (Set StateID)
        startDecoder =
            D.field "start" <| decodeSet D.int

        finalDecoder : D.Decoder (Set StateID)
        finalDecoder =
            D.field "final" <| decodeSet D.int

        transNamesDecoder : D.Decoder TransitionNames
        transNamesDecoder =
            D.field "transNames" <| decodeDict D.int (decodeSet D.string)
    in
    D.map4 MachineFA
        machineCoreDecoder
        startDecoder
        finalDecoder
        transNamesDecoder




test : MachineFA
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

        statePositions =
            Dict.fromList [ ( 0, ( -50, 50 ) ), ( 1, ( 50, 50 ) ), ( 2, ( -50, -50 ) ), ( 3, ( 50, -50 ) ) ]

        stateNames =
            Dict.fromList [ ( 0, "q_0" ), ( 1, "q_1" ), ( 2, "q_2" ), ( 3, "q_3" ) ]

        transitionNames =
            Dict.fromList <| List.map (\( k, str ) -> ( k, Set.singleton str )) [ ( 0, "1" ), ( 1, "0" ), ( 2, "1" ), ( 3, "0" ), ( 4, "1" ), ( 5, "0" ), ( 6, "1" ), ( 7, "0" ) ]

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
    MachineFA (MachineCore q delta0 statePositions stateTransitions stateNames) start final transitionNames


view :
    Environment
    -> Model
    -> MachineFA
    -> Set StateID
    -> TransitionMistakes
    -> Shape Msg
view env model machine currentStates tMistakes =
    let
        newTrans =
            case List.head <| Dict.values machine.transitionNames of
                Just schar ->
                    FALabel (Set.toList schar |> renderString)

                Nothing ->
                    FALabel ""

        newTransID = List.head <| Dict.keys machine.transitionNames
    in
    group
        [ renderArrows machine model tMistakes
        , renderStates currentStates machine model env
        , MachineCommon.view env model machine.core currentStates tMistakes newTrans newTransID
        ]


renderArrows : MachineFA -> Model -> TransitionMistakes -> Shape Msg
renderArrows machine model tMistakes =
    let
        states =
            machine.core.q

        pos =
            machine.core.statePositions

        delta =
            machine.core.delta

        transPos =
            machine.core.stateTransitions

        stateList =
            Set.toList states

        edgeToList state =
            Dict.toList
                (case Dict.get state delta of
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
                                                case Dict.get chId machine.transitionNames of
                                                    Just setc ->
                                                        FALabel (Set.toList setc |> renderString)

                                                    _ ->
                                                        FALabel ""

                                            sel =
                                                case model of
                                                    SelectedArrow ( ss1, char, ss2 ) ->
                                                        char == chId

                                                    DraggingArrow ( ss1, char, ss2 ) _ ->
                                                        char == chId

                                                    _ ->
                                                        False

                                            mistake =
                                                getTransMistake tMistakes chId
                                        in
                                        group
                                            [ renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) 20 20 ch chId sel mistake s1 s2 model
                                            ]
                                    )
                                    [ ss ]
                            )
                            (edgeToList s1)
                        )
                    )
            )
            stateList


renderStates : Set StateID -> MachineFA -> Model -> Environment -> Shape Msg
renderStates currentStates machine model env =
    let
        states =
            machine.core.q

        pos =
            machine.core.statePositions

        finals =
            machine.final

        stateList =
            Set.toList states

        getPos state =
            case Dict.get state pos of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )

        thickness state =
            if Set.member state currentStates then
                2

            else
                1

        stateName sId =
            case Dict.get sId machine.core.stateNames of
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
                        |> filled blank
                        |> notifyEnterAt (StartMouseOverRim sId)
                        |> notifyMouseMoveAt (StartMouseOverRim sId)
                    , circle 20
                        |> filled blank
                        |> addOutline (solid (thickness sId)) black
                        |> notifyMouseDownAt (StartDragging sId)
                    , if Set.member sId finals then
                        circle 17
                            |> outlined (solid (thickness sId)) black

                      else
                        group []
                    , case model of
                        EditingStateLabel st str ->
                            if st == sId then
                                textBox str
                                    (if String.length str == 0 then
                                        34

                                     else
                                        6 * toFloat (String.length str)
                                    )
                                    20
                                    "LaTeX"
                                    (EditLabel sId)

                            else
                                group
                                    [ latex 25 18 "none" (stateName sId) AlignCentre
                                        |> move ( 0, 9 )
                                    ]

                        _ ->
                            group
                                [ latex 25 18 "none" (stateName sId) AlignCentre
                                    |> move ( 0, 9 )
                                ]
                    , case model of
                        SelectedState st ->
                            if st == sId then
                                circle 20.75
                                    |> outlined (solid 1.5) lightBlue

                            else
                                group []

                        MousingOverRim st ( x, y ) ->
                            let
                                ( x0, y0 ) =
                                    getPos st

                                ( dx, dy ) =
                                    ( x - x0, y - y0 )
                            in
                            if st == sId then
                                group
                                    [ circle 500
                                        |> filled blank
                                        |> notifyEnter StopMouseOverRim
                                    , group
                                        [ circle 7
                                            |> filled white
                                            |> addOutline (solid 0.5) black
                                        , rect 8 1.5 |> filled black
                                        , rect 1.5 8 |> filled black
                                        ]
                                        |> notifyMouseMoveAt MoveMouseOverRim
                                        |> notifyLeave StopMouseOverRim
                                        |> move ( 20 * cos (atan2 dy dx), 20 * sin (atan2 dy dx) )
                                    ]

                            else
                                group []

                        AddingArrowOverOtherState _ _ st ->
                            if st == sId then
                                circle 21.5
                                    |> outlined (solid 3) finsmLightBlue
                                    |> notifyLeave StopMouseOverRim

                            else
                                group []

                        _ ->
                            group []
                    ]
                    |> move (getPos sId)
                    |> (case model of
                            EditingStateLabel _ _ ->
                                identity

                            _ ->
                                if not env.holdingShift then
                                    notifyMouseDownAt (StartDragging sId)

                                else
                                    notifyTap (TapState sId)
                       )
            )
            stateList
