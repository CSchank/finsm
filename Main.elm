module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)
import Debug exposing (log)
import Html as H exposing (Html, node, input)
import Html.Attributes exposing (placeholder, value, style, attribute)
import Html.Events exposing (onInput)
import Json.Encode
import Debug exposing (log)
import Browser exposing(UrlRequest)
import Browser.Events
import Browser.Dom
import Tuple exposing (first, second)
import Task
import Url exposing(Url, percentEncode)
import GraphicSVG.App exposing(GetKeyState, appWithTick, Keys(..), KeyState(..), AppWithTick)


type Msg
    = Tick Float GetKeyState
    | Step
    | StartDragging State ( Float, Float )
    | StartDraggingArrow State State
    | SelectArrow ( State, State )
    | MouseOverStateLabel State
    | MouseLeaveStateLabel State
    | SelectStateLabel State
    | EditLabel State String
    | Drag ( Float, Float )
    | StopDragging
    | WindowSize ( Int, Int )
    | UrlChange Url
    | UrlRequest UrlRequest


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
    | DraggingState State ( Float, Float )
    | MousingOverStateLabel State
    | EditingStateLabel State
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
    Set.foldl (\curr ss -> Set.union ss (delta d ch curr)) Set.empty states


test : Machine
test =
    let
        q =
            Set.fromList [ "q_0", "q_1", "q_2", "q_3" ]

        sigma =
            Set.fromList [ "0", "1" ]

        delta0 =
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
        Machine q sigma delta0 start final

main : AppWithTick () Model Msg
main =
    appWithTick Tick
        { init = \flags url key ->
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
            , Task.perform (\vp -> WindowSize ( round vp.viewport.width, round vp.viewport.height )) Browser.Dom.getViewport
            )
        , update = update
        , view =\ m ->  { body = view m, title = "finSM: create and simulate finite state machines" }
        , subscriptions = \_ -> Browser.Events.onResize (\w h -> WindowSize ( w, h ))
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


update msg model =
    let
        ch =
            case (Array.get model.inputAt model.input) of
                Just c ->
                    c

                Nothing ->
                    ""
    in
        case msg of
            Tick t ( getKeyState, _, _ ) ->
                ( { model
                    | appState =
                        case model.appState of
                            EditingStateLabel st ->
                                if (getKeyState Enter == JustDown) then
                                    Regular
                                else
                                    model.appState

                            _ ->
                                model.appState
                  }
                , Cmd.none
                )

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

            StartDragging st ( x, y ) ->
                let
                    ( sx, sy ) =
                        case (Dict.get st model.statePositions) of
                            Just ( xx, yy ) ->
                                ( xx, yy )

                            Nothing ->
                                ( 0, 0 )
                in
                    ( { model | appState = DraggingState st ( x - sx, y - sy ) }, Cmd.none )

            StartDraggingArrow st1 st2 ->
                ( { model | appState = DraggingArrow ( st1, st2 ) }, Cmd.none )

            StopDragging ->
                ( { model | appState = Regular }, Cmd.none )

            SelectArrow ( s0, s1 ) ->
                ( { model | appState = SelectedArrow ( s0, s1 ) }, Cmd.none )

            Drag ( x, y ) ->
                case model.appState of
                    DraggingState st ( ox, oy ) ->
                        let
                            ( sx, sy ) =
                                case (Dict.get st model.statePositions) of
                                    Just ( xx, yy ) ->
                                        ( xx, yy )

                                    Nothing ->
                                        ( 0, 0 )
                        in
                            ( { model
                                | statePositions = updateStatePos st ( x - ox, y - oy ) model.statePositions
                              }
                            , Cmd.none
                            )

                    DraggingArrow ( s1, s2 ) ->
                        let
                            ( x0, y0 ) =
                                case (Dict.get s1 model.statePositions) of
                                    Just ( xx, yy ) ->
                                        ( xx, yy )

                                    Nothing ->
                                        ( 0, 0 )

                            ( x1, y1 ) =
                                case (Dict.get s2 model.statePositions) of
                                    Just ( xx, yy ) ->
                                        ( xx, yy )

                                    Nothing ->
                                        ( 0, 0 )

                            theta =
                                -1 * atan2 (y1 - y0) (x1 - x0)

                            ( mx, my ) =
                                ( (x0 + x1) / 2, (y0 + y1) / 2 )

                            ( nx, ny ) =
                                sub ( x, y ) ( mx, my )

                            nprot =
                                ( nx * cos theta - ny * sin theta, nx * sin theta + ny * cos theta )
                        in
                            ( { model | stateTransitions = Dict.insert ( s1, s2 ) nprot model.stateTransitions }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            MouseOverStateLabel st ->
                ( { model | appState = MousingOverStateLabel st }, Cmd.none )

            MouseLeaveStateLabel st ->
                ( { model | appState = Regular }, Cmd.none )

            SelectStateLabel st ->
                ( { model | appState = EditingStateLabel st }, Cmd.none )

            EditLabel st lbl ->
                ( model, Cmd.none )

            WindowSize ( w, h ) ->
                ( { model | windowSize = ( w, h ) }, Cmd.none )
        
            UrlChange _ -> ( model, Cmd.none )
            
            UrlRequest _ -> ( model, Cmd.none )



--renameState : State -> String ->


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

        editIcon =
            group
                [ --square 5 |> outlined (solid 1) black
                  rect 5 2
                    |> filled blue
                    |> rotate (degrees 45)
                    |> move ( 3, 3 )
                , triangle 1
                    |> filled blue
                    |> rotate (degrees -15)
                ]
    in
        group <|
            List.map
                (\state ->
                    group
                        [ circle 20
                            |> outlined (solid (thickness state)) black
                            |> notifyMouseDownAt (StartDragging state)
                        , if (Set.member state finals) then
                            circle 17
                                |> outlined (solid (thickness state)) black
                                |> notifyMouseDownAt (StartDragging state)
                          else
                            group []

                        --, html (H.div [] [ textHtml """<svg xmlns:xlink="http://www.w3.org/1999/xlink" width="2.313ex" height="1.861ex" viewBox="-38.5 -533.5 995.9 801.3" role="img" focusable="false" style="vertical-align: -0.622ex; margin-left: -0.089ex;" aria-hidden="true"><g stroke="currentColor" fill="currentColor" stroke-width="0" transform="matrix(1 0 0 -1 0 0)"><use xlink:href="#MJMATHI-70" x="0" y="0"></use><use transform="scale(0.707)" xlink:href="#MJMAIN-30" x="712" y="-213"></use></g></svg>""" ]) |> move ( -5, 10 )
                        , case model.appState of
                            EditingStateLabel st ->
                                if st == state then
                                    textBox state 20 20 "LaTeX" (EditLabel state)
                                else
                                    (latex 20 20 state)
                                        |> move ( -8, 9 )
                                        |> scale 0.9
                                        |> notifyEnter (MouseOverStateLabel state)
                                        |> notifyLeave (MouseLeaveStateLabel state)
                                        |> notifyTap (SelectStateLabel state)

                            _ ->
                                (latex 20 20 state)
                                    |> move ( -8, 9 )
                                    |> scale 0.9
                                    |> notifyEnter (MouseOverStateLabel state)
                                    |> notifyLeave (MouseLeaveStateLabel state)
                                    |> notifyTap (SelectStateLabel state)
                        , case model.appState of
                            MousingOverStateLabel st ->
                                if state == st then
                                    editIcon |> scale 0.75 |> move ( 5, 5.5 )
                                else
                                    group []

                            _ ->
                                group []

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
                )
                stateList


textBox : String -> Float -> Float -> String -> (String -> Msg) -> Shape Msg
textBox txt w h place msg =
    move ( -w / 2, h / 2 ) <|
        html (w * 1.5) (h * 1.5) <|
            input
                [ placeholder place
                , onInput msg
                , value txt
                , style "width" (String.fromFloat w ++ "px")
                , style "height" (String.fromFloat h ++ "px")
                , style "margin-top" "1px"
                ]
                []


latex w h txt =
    html w h <| H.img [ Html.Attributes.attribute "onError" ("this.src='" ++ latexurl "\\LaTeX?" ++ "'")
                      , Html.Attributes.src (latexurl txt)
                      , style "width" "100%"
                      , style "height" "100%"
                      , attribute "user-select" "none"
                       ] []


latexurl : String -> String
latexurl lx =
    "https://do.schankula.ca/latex/render/" ++ percentEncode lx
    --"http://localhost:8001/latex/render/" ++ percentEncode lx


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
                , latex 12 12 char
                    |> move ( -6, 7 )
                    |> move (p ( xx0, yy0 ) ( mx, my ) ( xx1, yy1 ) 0.5)
                    |> move
                        ( -offset * sin theta
                        , offset * cos theta
                        )
                ]
                |> notifyMouseDown (SelectArrow ( s1, s2 ))

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
                        |> notifyMouseDown (StartDraggingArrow s1 s2)
                        |> notifyMouseMoveAt Drag
                    ]
              else
                group []
            ]


renderArrows states del pos transPos model =
    let
        stateList =
            Set.toList states

        edgeToList state =
            Dict.toList
                (case (Dict.get state del) of
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
                                (\( ch, ss ) ->
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
                                        (Set.toList ss)
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
            add p0 p2

        t =
            log "t" <| dot (sub p0 p1) (sub p3 (mult p1 2)) / (dot p3 p3 - 4 * dot (p1) (sub p3 p1))
    in
        p p0 p1 p2 t


p p0 p1 p2 t =
    add (mult p0 ((1 - t) ^ 2)) (add (mult (mult (mult p1 t) (1 - t)) 2) (mult p2 (t ^ 2)))


add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )

mult (x, y) s =
    (x * s, y * s)

sub ( x0, y0 ) ( x1, y1 ) =
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
            winX
            --winX
            winY
            --winY
            [ group [ renderStates model.machine.q model.states model.machine.final model.statePositions model ] |> move ( 0, 0 )
            , renderArrows model.machine.q model.machine.delta model.statePositions model.stateTransitions model

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
            , text (Debug.toString model.appState) |> filled black |> move ( 0, -150 )
            , text (Debug.toString model.windowSize) |> filled black |> move ( 0, -170 )
            , case model.appState of
                DraggingState _ _ ->
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
