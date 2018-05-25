module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)
import Debug exposing (log)
import Katex as K exposing (Latex, human, inline, display, generate)
import Html as H exposing (Html, node)
import Html.Attributes
import Json.Encode
import Debug exposing (log)
import Window
import Tuple exposing (first, second)
import Task


type Msg
    = Tick Float GetKeyState
    | Step
    | StartDragging State
    | StartDraggingArrow State State
    | SelectArrow ( State, State )
    | Drag ( Float, Float )
    | StopDragging
    | WindowSize ( Int, Int )


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


type ApplicationState
    = Regular
    | DraggingState State
    | SelectedArrow ( State, State )
    | DraggingArrow ( State, State )


type alias Model =
    { appState : ApplicationState
    , machine : Machine
    , states : Set State
    , input : Array Character
    , inputAt : Int
    , statePositions : StatePositions
    , stateTransitions : StateTransitions
    , windowSize : ( Int, Int )
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
            Set.fromList [ "q_0", "q_1", "q_2", "q_3" ]

        sigma =
            Set.fromList [ "0", "1" ]

        delta =
            Dict.fromList
                [ ( "q_0", Dict.fromList [ ( "1", Set.singleton "q_1" ), ( "0", Set.singleton "q_2" ) ] )
                , ( "q_1", Dict.fromList [ ( "1", Set.singleton "q_0" ), ( "0", Set.singleton "q_3" ) ] )
                , ( "q_2", Dict.fromList [ ( "1", Set.singleton "q_3" ), ( "0", Set.singleton "q_0" ) ] )
                , ( "q_3", Dict.fromList [ ( "1", Set.singleton "q_2" ), ( "0", Set.singleton "q_1" ) ] )
                ]

        start =
            Set.fromList [ "q_0" ]

        final =
            Set.fromList [ "q_0" ]
    in
        Machine q sigma delta start final


main =
    cmdApp Tick
        { init =
            ( { appState = Regular
              , machine = test
              , states = test.start
              , input = Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ]
              , inputAt = 0
              , statePositions = Dict.fromList [ ( "q_0", ( -50, 50 ) ), ( "q_1", ( 50, 50 ) ), ( "q_2", ( -50, -50 ) ), ( "q_3", ( 50, -50 ) ) ]
              , stateTransitions =
                    Dict.fromList
                        [ ( ( "q_0", "q_1" ), ( 0, 10 ) )
                        , ( ( "q_1", "q_0" ), ( 0, 10 ) )
                        , ( ( "q_0", "q_2" ), ( 0, 10 ) )
                        , ( ( "q_2", "q_0" ), ( 0, 10 ) )
                        , ( ( "q_2", "q_3" ), ( 0, 10 ) )
                        , ( ( "q_3", "q_2" ), ( 0, 10 ) )
                        , ( ( "q_1", "q_3" ), ( 0, 10 ) )
                        , ( ( "q_3", "q_1" ), ( 0, 10 ) )
                        ]
              , windowSize = ( 500, 500 )
              }
            , Task.perform (\size -> WindowSize ( size.width, size.height )) Window.size
            )
        , update = update
        , view = view
        , subscriptions = \_ -> Window.resizes (\size -> WindowSize ( size.width, size.height ))
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

            StartDragging st ->
                ( { model | appState = DraggingState st }, Cmd.none )

            StartDraggingArrow st1 st2 ->
                ( { model | appState = DraggingArrow ( st1, st2 ) }, Cmd.none )

            StopDragging ->
                ( { model | appState = Regular }, Cmd.none )

            SelectArrow ( s0, s1 ) ->
                ( { model | appState = SelectedArrow ( s0, s1 ) }, Cmd.none )

            Drag ( x, y ) ->
                case model.appState of
                    DraggingState st ->
                        let
                            ( sx, sy ) =
                                case (Dict.get st model.statePositions) of
                                    Just ( x, y ) ->
                                        ( x, y )

                                    Nothing ->
                                        ( 0, 0 )
                        in
                            ( { model
                                | statePositions = updateStatePos st ( x, y ) model.statePositions
                              }
                            , Cmd.none
                            )

                    DraggingArrow ( s1, s2 ) ->
                        let
                            ( x0, y0 ) =
                                case (Dict.get s1 model.statePositions) of
                                    Just ( x, y ) ->
                                        ( x, y )

                                    Nothing ->
                                        ( 0, 0 )

                            ( x1, y1 ) =
                                case (Dict.get s2 model.statePositions) of
                                    Just ( x, y ) ->
                                        ( x, y )

                                    Nothing ->
                                        ( 0, 0 )

                            theta =
                                atan2 (y1 - y0) (x1 - x0)

                            ( mx, my ) =
                                ( (x0 + x1) / 2, (y0 + y1) / 2 )

                            ( nx, ny ) =
                                ( x, y ) ~~~ ( mx, my )

                            nprot =
                                ( nx * cos theta + ny * sin theta, nx * sin theta + ny * cos theta )
                        in
                            ( { model | stateTransitions = Dict.insert ( s1, s2 ) nprot model.stateTransitions }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            WindowSize ( w, h ) ->
                ( { model | windowSize = ( w, h ) }, Cmd.none )


updateStatePos : State -> ( Float, Float ) -> StatePositions -> StatePositions
updateStatePos st ( x, y ) pos =
    Dict.update st
        (\m ->
            case m of
                Just _ ->
                    Just ( x, y )

                Nothing ->
                    Nothing
        )
        pos


updateArrowPos : State -> Float -> StateTransitions -> StateTransitions
updateArrowPos st angle pos =
    Dict.map
        (\( st0, st1 ) ( x, y ) ->
            if st0 == st then
                ( x * cos angle, y * sin angle )
            else if st1 == st then
                ( x * cos (-angle), y * sin (-angle) )
            else
                ( x, y )
        )
        pos


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


textHtml : String -> Html msg
textHtml t =
    H.span
        [ Json.Encode.string t
            |> Html.Attributes.property "innerHTML"
        ]
        []


renderStates states currents finals pos model =
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

                        --, html (H.div [] [ textHtml """<svg xmlns:xlink="http://www.w3.org/1999/xlink" width="2.313ex" height="1.861ex" viewBox="-38.5 -533.5 995.9 801.3" role="img" focusable="false" style="vertical-align: -0.622ex; margin-left: -0.089ex;" aria-hidden="true"><g stroke="currentColor" fill="currentColor" stroke-width="0" transform="matrix(1 0 0 -1 0 0)"><use xlink:href="#MJMATHI-70" x="0" y="0"></use><use transform="scale(0.707)" xlink:href="#MJMAIN-30" x="712" y="-213"></use></g></svg>""" ]) |> move ( -5, 10 )
                        , (html <| H.img [ Html.Attributes.src (latexurl state) ] []) |> move ( -7, 7 )

                        --, text state |> centered |> filled black |> move ( 0, -3 )
                        {--, case model.appState of
                            DraggingState st ->
                                if st == state then
                                    circle 21 |> outlined (solid 1) lightBlue
                                else
                                    group []

                            a ->
                                group []--}
                        ]
                        |> move (getPos state)
                        |> notifyMouseDown (StartDragging state)
                )
                stateList


latexurl : String -> String
latexurl latex =
    "https://do.schankula.ca/latex/render/" ++ latex


viewLatex : Latex -> Html a
viewLatex =
    let
        htmlGenerator isDisplayMode stringLatex =
            case isDisplayMode of
                Just True ->
                    H.div [ Html.Attributes.attribute "xmlns" "http://www.w3.org/1999/xhtml" ] [ H.text stringLatex ]

                _ ->
                    H.span [ Html.Attributes.attribute "xmlns" "http://www.w3.org/1999/xhtml" ] [ H.text stringLatex ]
    in
        generate htmlGenerator


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


renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) r0 r1 char sel s1 s2 =
    let
        ( tx, ty ) =
            --tangent between to and from states
            ( x2 - x0, y2 - y0 )

        theta =
            -1 * atan2 ty tx

        ( mx, my ) =
            --pull point
            ( (x2 + x0) / 2 + x1 * cos theta + y1 * sin theta, (y2 + y0) / 2 + y1 * cos theta + x1 * sin theta )

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
    in
        group
            [ arrow ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 )
                |> notifyMouseDown (SelectArrow ( s1, s2 ))
            , text char |> centered |> filled black |> move ( mx, my )
            , if sel then
                group
                    [ line ( xx0, yy0 ) ( mx, my ) |> outlined (dotted 1) black
                    , line ( xx1, yy1 ) ( mx, my ) |> outlined (dotted 1) black
                    , circle 3
                        |> filled red
                        |> move ( mx, my )
                        |> notifyMouseDown (StartDraggingArrow s1 s2)
                        |> notifyMouseMoveAt Drag
                    ]
              else
                group []
            ]


renderArrows states delta pos transPos model =
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
                                    List.map
                                        (\s2 ->
                                            let
                                                ( x0, y0 ) =
                                                    (getPos s1)

                                                ( x1, y1 ) =
                                                    (getTransPos ( s1, s2 ))

                                                ( x2, y2 ) =
                                                    (getPos s2)

                                                sel =
                                                    case model.appState of
                                                        SelectedArrow ( ss1, ss2 ) ->
                                                            ( ss1, ss2 ) == ( s1, s2 )

                                                        DraggingArrow ( ss1, ss2 ) ->
                                                            ( ss1, ss2 ) == ( s1, s2 )

                                                        _ ->
                                                            False
                                            in
                                                group
                                                    [ renderArrow ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) 20 20 ch sel s1 s2
                                                    ]
                                        )
                                        (Set.toList states)
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
        p p0 p1 p2 t


p p0 p1 p2 t =
    (p0 *** ((1 - t) ^ 2)) +++ ((p1 *** t) *** (1 - t) *** 2) +++ (p2 *** (t ^ 2))


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

        winX =
            (toFloat <| first model.windowSize)

        winY =
            (toFloat <| second model.windowSize)
    in
        collage
            1280
            --winX
            720
            --winY
            [ group [ renderArrows model.machine.q model.machine.delta model.statePositions model.stateTransitions model, renderStates model.machine.q model.states model.machine.final model.statePositions model ] |> move ( 0, 0 )

            --, renderTape model.input model.inputAt |> move ( 0, 0 )
            {- , text ("Accepted: " ++ toString accepted)
               |> centered
               |> filled
                   (if accepted then
                       green
                    else
                       red
                   )
               |> move ( 0, -60 )
            -}
            , group [ roundedRect 30 30 10 |> filled lightGreen, triangle 10 |> filled white ] |> move ( 0, -100 ) |> notifyTap Step
            , text (toString model.appState) |> filled black |> move ( 0, -150 )
            , case model.appState of
                DraggingState _ ->
                    rect winX winY
                        |> filled blank
                        |> notifyMouseMoveAt Drag
                        |> notifyMouseUp StopDragging

                DraggingArrow _ ->
                    rect winX winY
                        |> filled blank
                        |> notifyMouseMoveAt Drag
                        |> notifyMouseUp StopDragging

                _ ->
                    group []
            ]
