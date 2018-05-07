module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)
import Debug exposing (log)


type Msg
    = Tick Float GetKeyState
    | Step


type alias State =
    String


type alias Character =
    String


type alias Delta =
    Dict State (Dict Character (Set State))


type alias InputTape =
    Array Character


type alias StatePositions =
    Dict State ( Float, Float )


type alias StateTransitions =
    Dict ( State, State ) ( Float, Float )


type alias Model =
    { machine : Machine
    , states : Set State
    , input : Array Character
    , inputAt : Int
    , statePositions : StatePositions
    , stateTransitions : StateTransitions
    }


type alias Machine =
    { q : Set State
    , sigma : Set Character
    , delta : Delta
    , start : Set State
    , final : Set State
    }


delta : Delta -> Character -> State -> Set State
delta d ch state =
    case (Dict.get state d) of
        Just charMap ->
            case (Dict.get ch charMap) of
                Just stateSet ->
                    stateSet

                Nothing ->
                    Set.empty

        Nothing ->
            Set.empty


deltaHat : Delta -> Character -> Set State -> Set State
deltaHat d ch states =
    Set.foldl (\curr states -> Set.union states (delta d ch curr)) Set.empty states


test : Machine
test =
    let
        q =
            Set.fromList [ "q0", "q1", "q2", "q3" ]

        sigma =
            Set.fromList [ "0", "1" ]

        delta =
            Dict.fromList
                [ ( "q0", Dict.fromList [ ( "1", Set.singleton "q1" ), ( "0", Set.singleton "q2" ) ] )
                , ( "q1", Dict.fromList [ ( "1", Set.singleton "q0" ), ( "0", Set.singleton "q3" ) ] )
                , ( "q2", Dict.fromList [ ( "1", Set.singleton "q3" ), ( "0", Set.singleton "q0" ) ] )
                , ( "q3", Dict.fromList [ ( "1", Set.singleton "q2" ), ( "0", Set.singleton "q1" ) ] )
                ]

        start =
            Set.fromList [ "q0" ]

        final =
            Set.fromList [ "q0" ]
    in
        Machine q sigma delta start final


main =
    cmdApp Tick
        { init =
            ( { machine = test
              , states = test.start
              , input = Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ]
              , inputAt = 0
              , statePositions = Dict.fromList [ ( "q0", ( -50, 50 ) ), ( "q1", ( 50, 50 ) ), ( "q2", ( -50, -50 ) ), ( "q3", ( 50, -50 ) ) ]
              , stateTransitions =
                    Dict.fromList
                        [ ( ( "q0", "q1" ), ( 0, 15 ) )
                        , ( ( "q1", "q0" ), ( 0, -15 ) )
                        , ( ( "q0", "q2" ), ( 15, 0 ) )
                        , ( ( "q2", "q0" ), ( -15, 0 ) )
                        , ( ( "q2", "q3" ), ( 0, 15 ) )
                        , ( ( "q3", "q2" ), ( 0, -15 ) )
                        , ( ( "q1", "q3" ), ( 15, 0 ) )
                        , ( ( "q3", "q1" ), ( -15, 0 ) )
                        ]
              }
            , Cmd.none
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update msg model =
    let
        ch =
            case (Array.get model.inputAt model.input) of
                Just ch ->
                    ch

                Nothing ->
                    ""
    in
        case msg of
            Tick t _ ->
                ( model, Cmd.none )

            Step ->
                if ch /= "" then
                    ( { model
                        | states = deltaHat model.machine.delta ch model.states
                        , inputAt = model.inputAt + 1
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )


isAccept : Set State -> Set State -> InputTape -> Int -> Bool
isAccept states finals input inputAt =
    if inputAt == Array.length input then
        Set.size (Set.intersect states finals) > 0
    else
        False


renderTape input inputAt =
    let
        xpad =
            20
    in
        group
            (Array.toList
                (Array.indexedMap
                    (\n st ->
                        group
                            [ square xpad
                                |> outlined
                                    (solid
                                        (if inputAt == n then
                                            2
                                         else
                                            1
                                        )
                                    )
                                    black
                                |> move ( 0, 3 )
                            , text st
                                |> centered
                                |> filled black
                            ]
                            |> move ( toFloat n * xpad, 0 )
                    )
                    input
                )
            )
            |> move
                ( -xpad / 2 * toFloat (Array.length input - 1)
                , 0
                )



--renderStates : Set State -> StatePositions


renderStates states currents finals pos =
    let
        stateList =
            Set.toList states

        getPos state =
            case (Dict.get state pos) of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )

        thickness state =
            (if Set.member state currents then
                2
             else
                1
            )
    in
        group <|
            List.map
                (\state ->
                    group
                        [ circle 20
                            |> outlined (solid (thickness state)) black
                        , if (Set.member state finals) then
                            circle 17 |> outlined (solid (thickness state)) black
                          else
                            group []
                        , text state |> centered |> filled black |> move ( 0, -3 )
                        ]
                        |> move (getPos state)
                )
                stateList


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


renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) r0 r1 char =
    let
        ( mx, my ) =
            ( (x2 + x0) / 2 + x1, (y2 + y0) / 2 + y1 )

        ( dx0, dy0 ) =
            ( mx - x0, my - y0 )

        ( dx1, dy1 ) =
            ( mx - x2, my - y2 )

        ( xx0, yy0 ) =
            ( x0 + r0 * cos (atan2 dy0 dx0), y0 + r0 * sin (atan2 dy0 dx0) )

        ( xx1, yy1 ) =
            ( x2 + r1 * cos (atan2 dy1 dx1), y2 + r1 * sin (atan2 dy1 dx1) )
    in
        group
            [ arrow ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 )
            , text char |> centered |> filled black |> move ( mx, my )
            ]


renderArrows states delta pos transPos =
    let
        stateList =
            Set.toList states

        edgeToList state =
            Dict.toList
                (case (Dict.get state delta) of
                    Just d ->
                        d

                    Nothing ->
                        Dict.empty
                )

        getPos state =
            case (Dict.get state pos) of
                Just ( x, y ) ->
                    ( x, y )

                Nothing ->
                    ( 0, 0 )

        getTransPos ( s1, s2 ) =
            case (Dict.get ( s1, s2 ) transPos) of
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
                                (\( ch, states ) ->
                                    List.map (\s2 -> renderArrow (getPos s1) (getTransPos ( s1, s2 )) (getPos s2) 20 20 ch) (Set.toList states)
                                )
                                (edgeToList s1)
                            )
                        )
                )
                stateList


vertex ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        p0 =
            ( x0, y0 )

        p1 =
            ( x1, y1 )

        p2 =
            ( x2, y2 )

        p3 =
            p0 +++ p2

        t =
            log "t" <| dot (p0 ~~~ p1) (p3 ~~~ (p1 *** 2)) / (dot p3 p3 - 4 * dot (p1) (p3 ~~~ p1))
    in
        (p0 *** ((1 - t) ^ 2)) +++ ((p1 *** t) *** (1 - t)) +++ (p2 *** (t ^ 2))


(+++) ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


(***) ( x0, y0 ) x =
    ( x0 * x, y0 * x )


(~~~) ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )


dot ( x0, y0 ) ( x1, y1 ) =
    x0 * x1 + y0 * y1



--List.map (\state -> )


view model =
    let
        accepted =
            isAccept model.states model.machine.final model.input model.inputAt
    in
        collage 500
            500
            [ group [ renderArrows model.machine.q model.machine.delta model.statePositions model.stateTransitions, renderStates model.machine.q model.states model.machine.final model.statePositions ] |> move ( 0, 100 )
            , renderTape model.input model.inputAt |> move ( 0, 0 )
            , text ("Accepted: " ++ toString accepted)
                |> centered
                |> filled
                    (if accepted then
                        green
                     else
                        red
                    )
                |> move ( 0, -60 )
            , group [ roundedRect 30 30 10 |> filled lightGreen, triangle 10 |> filled white ] |> move ( 0, -100 ) |> notifyTap Step
            , curve ( 0, 0 ) [ Pull ( 50, 10 ) ( 100, 0 ) ] |> outlined (solid 1) black
            , circle 2 |> filled yellow |> move (vertex ( 0, 0 ) ( 50, 10 ) ( 100, 0 ))
            , text (toString (vertex ( -1, 1 ) ( 0, -1 ) ( 1, 1 ))) |> filled red |> move ( 0, -50 )
            ]
