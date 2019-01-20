module Machine exposing (Character, Delta, Machine, Model(..), Msg(..), StateID, StateNames, StatePositions, StateTransitions, TransitionID, TransitionNames, arrow, renderArrow, renderArrows, renderStates, test, textBox, view)

import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Html as H exposing (Html, input, node)
import Html.Attributes exposing (attribute, id, placeholder, style, value)
import Html.Events exposing (onInput)
import Set exposing (Set)


type alias StateID =
    Int


type alias StatePositions =
    Dict StateID ( Float, Float )


type alias TransitionID =
    Int


type alias StateNames =
    Dict StateID String


type alias TransitionNames =
    Dict TransitionID String


type alias StateTransitions =
    Dict ( StateID, TransitionID, StateID ) ( Float, Float )


type alias Delta =
    Dict StateID (Dict TransitionID StateID)


type alias Character =
    String


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
    | DraggingState StateID ( Float, Float )
    | SelectedState StateID
    | MousingOverRim StateID ( Float, Float )
    | AddingArrow StateID ( Float, Float )
    | AddingArrowOverOtherState StateID ( Float, Float ) StateID
    | MousingOverStateLabel StateID
    | MousingOverTransitionLabel TransitionID
    | EditingStateLabel StateID String
    | EditingTransitionLabel TransitionID String
    | SelectedArrow ( StateID, TransitionID, StateID )
    | DraggingArrow ( StateID, TransitionID, StateID )
    | CreatingNewArrow StateID


type Msg
    = StartDragging StateID ( Float, Float )
    | StartDraggingArrow ( StateID, TransitionID, StateID )
    | StartMouseOverRim StateID ( Float, Float )
    | MoveMouseOverRim ( Float, Float )
    | StopMouseOverRim
    | SelectArrow ( StateID, TransitionID, StateID )
    | MouseOverStateLabel StateID
    | MouseOverTransitionLabel TransitionID
    | MouseLeaveLabel
    | SelectStateLabel StateID
    | SelectTransitionLabel StateID
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
            Dict.fromList [ ( 0, "1" ), ( 1, "0" ), ( 2, "1" ), ( 3, "0" ), ( 4, "1" ), ( 5, "0" ), ( 6, "1" ), ( 7, "0" ), ( 8, "1" ) ]

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


view : Environment -> Model -> Machine -> Set StateID -> Shape Msg
view env model machine currentStates =
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
        [ renderArrows machine model
        , renderStates currentStates machine model
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
                            Just char ->
                                char

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys machine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0
                in
                renderArrow s0Pos ( 0, 0 ) ( x, y ) 20 0 newTrans newTransID False s -1 model

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
                            Just char ->
                                char

                            Nothing ->
                                " "

                    newTransID =
                        case List.head <| Dict.keys machine.transitionNames of
                            Just char ->
                                char

                            Nothing ->
                                0
                in
                renderArrow s0Pos ( 0, 0 ) s1Pos 20 20 newTrans newTransID False s -1 model

            _ ->
                group []
        , case model of
            DraggingState _ _ ->
                dragRegion

            DraggingArrow _ ->
                dragRegion

            AddingArrow _ _ ->
                dragRegion

            AddingArrowOverOtherState _ _ _ ->
                dragRegion

            _ ->
                group []
        ]



--These two functions will eventually become part of GraphicSVG in some form


textBox : String -> Float -> Float -> String -> (String -> Msg) -> Shape Msg
textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ id "input"
                , placeholder place
                , onInput msg
                , value txt
                , style "width" (String.fromFloat w ++ "px")
                , style "height" (String.fromFloat h ++ "px")
                , style "margin-top" "1px"
                , style "font-family" "monospace"
                ]
                []


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


renderArrow : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> Character -> TransitionID -> Bool -> StateID -> StateID -> Model -> Shape Msg
renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) r0 r1 char charID sel s1 s2 model =
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
            ( x0 + r0 * cos (atan2 dy0 dx0), y0 + r0 * sin (atan2 dy0 dx0) )

        ( xx1, yy1 ) =
            --to state position (with radius accounted for)
            ( x2 + r1 * cos (atan2 dy1 dx1), y2 + r1 * sin (atan2 dy1 dx1) )

        offset =
            if y1 > 0 then
                8

            else
                -8
    in
    group
        [ group
            [ arrow ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 )
                |> notifyMouseDown (SelectArrow ( s1, charID, s2 ))
            , group
                [ case model of
                    EditingTransitionLabel tId str ->
                        if tId == charID then
                            textBox str
                                (if String.length str == 0 then
                                    34

                                 else
                                    6 * toFloat (String.length str)
                                )
                                20
                                "LaTeX"
                                (EditLabel tId)

                        else
                            latex 50 12 char AlignCentre

                    _ ->
                        latex 50 12 char AlignCentre
                , case model of
                    MousingOverTransitionLabel tId ->
                        if tId == charID then
                            group
                                [ editIcon |> move ( 10, 0 )
                                , rect 50 20
                                    |> filled blank
                                    |> notifyTap (SelectTransitionLabel charID)
                                ]

                        else
                            group []

                    Regular ->
                        group
                            [ rect 50 20
                                |> filled blank
                                |> notifyEnter (MouseOverTransitionLabel charID)
                            ]

                    _ ->
                        group []
                ]
                |> move ( 0, 7 )
                |> move (p ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 ) 0.5)
                |> move
                    ( -offset * sin theta
                    , offset * cos theta
                    )
                |> notifyLeave MouseLeaveLabel
            ]

        {- ( (x2 + x0)
               / 2
               + rx
               / 2
               + if rx > 0 then
                   10 + 0.08 * rx
                 else
                   -10 - 0.08 * rx
           , (y2 + y0)
               / 2
               + ry
               / 2
           )
        -}
        , if sel then
            group
                [ line ( xx0, yy0 ) ( mx, my ) |> outlined (dotted 1) black
                , line ( xx1, yy1 ) ( mx, my ) |> outlined (dotted 1) black
                , circle 3
                    |> filled red
                    |> move ( mx, my )
                    |> notifyMouseDown (StartDraggingArrow ( s1, charID, s2 ))
                    |> notifyMouseMoveAt Drag
                ]

          else
            group []
        ]


renderArrows : Machine -> Model -> Shape Msg
renderArrows machine model =
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
                                                    Just c ->
                                                        c

                                                    _ ->
                                                        ""

                                            sel =
                                                case model of
                                                    SelectedArrow ( ss1, char, ss2 ) ->
                                                        char == chId

                                                    DraggingArrow ( ss1, char, ss2 ) ->
                                                        char == chId

                                                    _ ->
                                                        False
                                        in
                                        group
                                            [ renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) 20 20 ch chId sel s1 s2 model
                                            ]
                                    )
                                    [ ss ]
                            )
                            (edgeToList s1)
                        )
                    )
            )
            stateList


renderStates : Set StateID -> Machine -> Model -> Shape Msg
renderStates currentStates machine model =
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
    in
    group <|
        List.map
            (\sId ->
                group
                    [ circle 21
                        |> outlined (solid 3) blank
                        |> notifyEnterAt (StartMouseOverRim sId)
                        |> notifyMouseMoveAt (StartMouseOverRim sId)
                    , circle 20
                        |> outlined (solid (thickness sId)) black
                        |> notifyMouseDownAt (StartDragging sId)
                    , if Set.member sId finals then
                        circle 17
                            |> outlined (solid (thickness sId)) black
                            |> notifyMouseDownAt (StartDragging sId)

                      else
                        group []
                    , case model of
                        SelectedState st ->
                            if st == sId then
                                circle 20.75
                                    |> outlined (solid 1.5) lightBlue
                                    |> notifyMouseDownAt (StartDragging sId)

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
                                        |> notifyMouseDownAt (StartDragging sId)
                                        |> notifyLeave StopMouseOverRim
                                        |> move ( 20 * cos (atan2 dy dx), 20 * sin (atan2 dy dx) )
                                    ]

                            else
                                group []

                        AddingArrowOverOtherState _ _ st ->
                            if st == sId then
                                circle 21.5
                                    |> outlined (solid 3) (rgb 112 190 255)
                                    |> notifyMouseDownAt (StartDragging sId)
                                    |> notifyLeave StopMouseOverRim

                            else
                                group []

                        _ ->
                            group []
                    , case model of
                        MousingOverStateLabel st ->
                            if sId == st then
                                editIcon |> scale 0.75 |> move ( 5, 5.5 )

                            else
                                group []

                        _ ->
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
                                    [ latex 25 18 (stateName sId) AlignCentre
                                        |> move ( 0, 9 )
                                    , rect 25 18
                                        |> filled blank
                                        |> notifyEnter (MouseOverStateLabel sId)
                                        |> notifyLeave MouseLeaveLabel
                                        |> notifyTap (SelectStateLabel sId)
                                    ]

                        _ ->
                            group
                                [ latex 25 18 (stateName sId) AlignCentre
                                    |> move ( 0, 9 )
                                , rect 25 18
                                    |> filled blank
                                    |> notifyEnter (MouseOverStateLabel sId)
                                    |> notifyLeave MouseLeaveLabel
                                    |> notifyTap (SelectStateLabel sId)
                                ]
                    ]
                    |> move (getPos sId)
            )
            stateList
