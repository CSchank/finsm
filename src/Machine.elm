module Machine exposing (..)

import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, id, placeholder, style, value)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Utils exposing (decodeDict, decodePair, decodeSet, decodeTriple, encodeDict, encodePair, encodeSet, encodeTriple, textBox)


type alias StateID =
    Int


type alias StatePositions =
    Dict StateID ( Float, Float )


type alias TransitionID =
    Int


type alias StateNames =
    Dict StateID String


type alias TransitionNames =
    Dict TransitionID (Set String)


type alias StateTransitions =
    Dict ( StateID, TransitionID, StateID ) ( Float, Float )


type alias Delta =
    Dict StateID (Dict TransitionID StateID)


type alias Character =
    String


type alias TransitionMistakes =
    Maybe (Set TransitionID)


machineEncoder : Machine -> E.Value
machineEncoder =
    machineEncoderV1


machineEncoderV1 : Machine -> E.Value
machineEncoderV1 machine =
    let
        transTriple =
            decodeTriple D.int D.int D.int

        qEncoder : Set StateID -> E.Value
        qEncoder =
            encodeSet E.int

        deltaEncoder : Delta -> E.Value
        deltaEncoder =
            encodeDict E.int (encodeDict E.int E.int)

        startEncoder : Set StateID -> E.Value
        startEncoder =
            encodeSet E.int

        finalEncoder : Set StateID -> E.Value
        finalEncoder =
            encodeSet E.int

        statePosEncoder : StatePositions -> E.Value
        statePosEncoder =
            encodeDict E.int (encodePair E.float E.float)

        transPosEncoder : StateTransitions -> E.Value
        transPosEncoder =
            encodeDict (encodeTriple E.int E.int E.int) (encodePair E.float E.float)

        stateNamesEncoder : StateNames -> E.Value
        stateNamesEncoder =
            encodeDict E.int E.string

        transNamesEncoder : TransitionNames -> E.Value
        transNamesEncoder =
            encodeDict E.int (encodeSet E.string)
    in
    E.object
        [ ( "q", qEncoder machine.q )
        , ( "delta", deltaEncoder machine.delta )
        , ( "start", startEncoder machine.start )
        , ( "final", finalEncoder machine.final )
        , ( "statePositions", statePosEncoder machine.statePositions )
        , ( "transPositions", transPosEncoder machine.stateTransitions )
        , ( "stateNames", stateNamesEncoder machine.stateNames )
        , ( "transNames", transNamesEncoder machine.transitionNames )
        , ( "v", E.int 1 )
        ]


machineDecoder : D.Decoder Machine
machineDecoder =
    D.field "v" D.int
        |> D.andThen
            (\v ->
                case v of
                    1 ->
                        machineDecoderV1

                    _ ->
                        D.fail <| "Invalid save metadata version " ++ String.fromInt v
            )


machineDecoderV1 : D.Decoder Machine
machineDecoderV1 =
    let
        transTriple =
            decodeTriple D.int D.int D.int

        qDecoder : D.Decoder (Set StateID)
        qDecoder =
            D.field "q" <| decodeSet D.int

        deltaDecoder : D.Decoder Delta
        deltaDecoder =
            D.field "delta" <| decodeDict D.int (decodeDict D.int D.int)

        startDecoder : D.Decoder (Set StateID)
        startDecoder =
            D.field "start" <| decodeSet D.int

        finalDecoder : D.Decoder (Set StateID)
        finalDecoder =
            D.field "final" <| decodeSet D.int

        statePosDecoder : D.Decoder StatePositions
        statePosDecoder =
            D.field "statePositions" <| decodeDict D.int (decodePair D.float D.float)

        transPosDecoder : D.Decoder StateTransitions
        transPosDecoder =
            D.field "transPositions" <| decodeDict transTriple (decodePair D.float D.float)

        stateNamesDecoder : D.Decoder StateNames
        stateNamesDecoder =
            D.field "stateNames" <| decodeDict D.int D.string

        transNamesDecoder : D.Decoder TransitionNames
        transNamesDecoder =
            D.field "transNames" <| decodeDict D.int (decodeSet D.string)
    in
    D.map8 Machine
        qDecoder
        deltaDecoder
        startDecoder
        finalDecoder
        statePosDecoder
        transPosDecoder
        stateNamesDecoder
        transNamesDecoder


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


type Model
    = Regular
    | DraggingState StateID ( Float, Float ) ( Float, Float )
    | SelectedState StateID
    | MousingOverRim StateID ( Float, Float )
    | AddingArrow StateID ( Float, Float )
    | AddingArrowOverOtherState StateID ( Float, Float ) StateID
    | MousingOverStateLabel StateID
    | MousingOverTransitionLabel TransitionID
    | EditingStateLabel StateID String
    | EditingTransitionLabel ( StateID, TransitionID, StateID ) String
    | SelectedArrow ( StateID, TransitionID, StateID )
    | DraggingArrow ( StateID, TransitionID, StateID ) ( Float, Float )
    | CreatingNewArrow StateID


type Msg
    = StartDragging StateID ( Float, Float )
    | StartDraggingArrow ( StateID, TransitionID, StateID ) ( Float, Float )
    | StartMouseOverRim StateID ( Float, Float )
    | MoveMouseOverRim ( Float, Float )
    | StopMouseOverRim
    | SelectArrow ( StateID, TransitionID, StateID )
    | MouseOverStateLabel StateID
    | MouseOverTransitionLabel TransitionID
    | MouseLeaveLabel
    | EditLabel StateID String
    | Drag ( Float, Float )
    | TapState StateID
    | StopDragging
    | Reset


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
    Machine q delta0 start final statePositions stateTransitions stateNames transitionNames


view : Environment -> Model -> Machine -> Set StateID -> TransitionMistakes -> Shape Msg
view env model machine currentStates tMistakes =
    let
        ( winX, winY ) =
            env.windowSize

        dragRegion =
            rect (toFloat winX) (toFloat winY)
                |> filled blank
                |> notifyMouseMoveAt Drag
                |> notifyMouseUp StopDragging
    in
    group
        [ renderArrows machine model tMistakes
        , renderStates currentStates machine model env
        , case model of
            AddingArrow s ( x, y ) ->
                let
                    s0Pos =
                        case Dict.get s machine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    newTrans =
                        case List.head <| Dict.values machine.transitionNames of
                            Just schar ->
                                Set.toList schar |> renderString

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys machine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0
                in
                renderArrow s0Pos ( 0, 0 ) ( x, y ) 20 0 newTrans newTransID False False s -1 model

            AddingArrowOverOtherState s ( x, y ) s1 ->
                let
                    s0Pos =
                        case Dict.get s machine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    s1Pos =
                        case Dict.get s1 machine.statePositions of
                            Just pos ->
                                pos

                            _ ->
                                ( 0, 0 )

                    newTrans =
                        case List.head <| Dict.values machine.transitionNames of
                            Just schar ->
                                Set.toList schar |> renderString

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys machine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0

                    pullPos =
                        if s == s1 then
                            ( 0, 50 )

                        else
                            ( 0, 0 )
                in
                renderArrow s0Pos pullPos s1Pos 20 20 newTrans newTransID False False s s1 model

            _ ->
                group []
        , case model of
            DraggingState _ _ _ ->
                dragRegion

            DraggingArrow _ _ ->
                dragRegion

            AddingArrow _ _ ->
                dragRegion

            AddingArrowOverOtherState _ _ _ ->
                dragRegion

            _ ->
                group []
        ]



--These two functions will eventually become part of GraphicSVG in some form


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


renderArrow :
    ( Float, Float )
    -> ( Float, Float )
    -> ( Float, Float )
    -> Float
    -> Float
    -> Character
    -> TransitionID
    -> Bool
    -> Bool
    -> StateID
    -> StateID
    -> Model
    -> Shape Msg
renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) r0 r1 char charID sel mistake s1 s2 model =
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
            if s1 == s2 then
                ( x0 + r0 * cos (atan2 dy0 dx0 + degrees 45), y0 + r0 * sin (atan2 dy0 dx0 + degrees 45) )

            else
                ( x0 + r0 * cos (atan2 dy0 dx0), y0 + r0 * sin (atan2 dy0 dx0) )

        ( xx1, yy1 ) =
            --to state position (with radius accounted for)
            if s1 == s2 then
                ( x0 + r0 * cos (atan2 dy0 dx0 - degrees 45), y0 + r0 * sin (atan2 dy0 dx0 - degrees 45) )

            else
                ( x2 + r1 * cos (atan2 dy1 dx1), y2 + r1 * sin (atan2 dy1 dx1) )

        tLblW =
            200

        off =
            if y1 > 0 then
                8

            else
                -8

        offset =
            ( -off * sin theta
            , off * cos theta
            )

        alignment =
            case labelPosition y1 theta of
                Above ->
                    AlignCentre

                Below ->
                    AlignCentre

                Left ->
                    AlignRight

                Right ->
                    AlignLeft
    in
    group
        [ group
            [ if s1 == s2 then
                let
                    mr =
                        sqrt ((mx - x0) ^ 2 + (my - y0) ^ 2)

                    mpl =
                        mr - r0

                    ppr =
                        sqrt (mr ^ 2 + mpl ^ 2)

                    beta =
                        atan2 ry rx

                    gamma =
                        atan2 mpl mr

                    ( x0s, y0s ) =
                        ( x0 + r0 * cos (beta - gamma), y0 + r0 * sin (beta - gamma) )

                    ( x1s, y1s ) =
                        ( x0 + r0 * cos (beta + gamma), y0 + r0 * sin (beta + gamma) )
                in
                group
                    [ curve ( x0s, y0s ) [ Pull ( x0 + ppr * cos (beta - gamma), y0 + ppr * sin (beta - gamma) ) ( mx, my ) ]
                        |> outlined (solid 1) black
                    , arrow ( mx, my ) ( x0 + ppr * cos (beta + gamma), y0 + ppr * sin (beta + gamma) ) ( x1s, y1s )
                    ]
                    |> notifyMouseDown (SelectArrow ( s1, charID, s2 ))

              else
                arrow ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 )
                    |> notifyMouseDown (SelectArrow ( s1, charID, s2 ))
            , group
                [ case model of
                    EditingTransitionLabel ( _, tId, _ ) str ->
                        if tId == charID then
                            textBox str
                                (if String.length str == 0 then
                                    40

                                 else
                                    8 * toFloat (String.length str) + 5
                                )
                                20
                                "LaTeX"
                                (EditLabel tId)

                        else
                            latex tLblW
                                12
                                (if mistake then
                                    "LightSalmon"

                                 else
                                    "none"
                                )
                                char
                                alignment

                    _ ->
                        latex tLblW
                            12
                            (if mistake then
                                "LightSalmon"

                             else
                                "none"
                            )
                            char
                            alignment
                , case model of
                    EditingTransitionLabel tId str ->
                        group []

                    _ ->
                        rect 50 20
                            |> filled blank
                            |> notifyTap (SelectArrow ( s1, charID, s2 ))
                ]
                |> (if s1 /= s2 then
                        move ( 0, 7 )
                            >> move (p ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 ) 0.5)
                            >> move offset

                    else
                        move ( mx, my + 12 )
                   )
                |> notifyLeave MouseLeaveLabel
            ]
        , if sel then
            group
                [ if s1 /= s2 then
                    line ( xx0, yy0 ) ( mx, my ) |> outlined (dotted 1) black

                  else
                    group []
                , if s1 /= s2 then
                    line ( xx1, yy1 ) ( mx, my ) |> outlined (dotted 1) black

                  else
                    group []
                , circle 3
                    |> filled finsmBlue
                    |> move ( mx, my )
                    |> notifyMouseDownAt (StartDraggingArrow ( s1, charID, s2 ))
                    |> notifyMouseMoveAt Drag
                ]

          else
            group []
        ]


renderArrows : Machine -> Model -> TransitionMistakes -> Shape Msg
renderArrows machine model tMistakes =
    let
        states =
            machine.q

        pos =
            machine.statePositions

        delta =
            machine.delta

        transPos =
            machine.stateTransitions

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
                                                        Set.toList setc |> renderString

                                                    _ ->
                                                        ""

                                            sel =
                                                case model of
                                                    SelectedArrow ( ss1, char, ss2 ) ->
                                                        char == chId

                                                    DraggingArrow ( ss1, char, ss2 ) _ ->
                                                        char == chId

                                                    _ ->
                                                        False

                                            -- Transition mistake function
                                            getTransMistake : TransitionMistakes -> TransitionID -> Bool
                                            getTransMistake transMistakes tId =
                                                case transMistakes of
                                                    Nothing ->
                                                        False

                                                    Just setOfMistakes ->
                                                        Set.member tId setOfMistakes

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


renderStates : Set StateID -> Machine -> Model -> Environment -> Shape Msg
renderStates currentStates machine model env =
    let
        states =
            machine.q

        pos =
            machine.statePositions

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
            case Dict.get sId machine.stateNames of
                Just n ->
                    n

                _ ->
                    ""

        startArrow =
            group
                [ arrow ( -15, 0 ) ( -5, 0 ) ( 0, 0 )
                , latex 25 18 "none" "\\text{start}" AlignRight |> move ( -16, 9 )
                ]
                |> move ( -20, 0 )
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
                                        40

                                     else
                                        8 * toFloat (String.length str) + 5
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
                    , if Set.member sId machine.start then
                        startArrow

                      else
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
