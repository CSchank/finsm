module Main exposing (..)

import GraphicSVG exposing (..)
import Random
import Array exposing (Array)
import List
import Set exposing (Set)
import Dict exposing (Dict)


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


type alias Model =
    { machine : Machine
    , states : Set State
    , input : Array Character
    , inputAt : Int
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
                , ( "q3", Dict.fromList [ ( "1", Set.singleton "q2" ), ( "0", Set.singleton "q1 " ) ] )
                ]

        start =
            Set.fromList [ "q0" ]

        final =
            Set.fromList [ "q0" ]
    in
        Machine q sigma delta start final


main =
    cmdApp Tick
        { init = ( { machine = test, states = test.start, input = Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ], inputAt = 0 }, Cmd.none )
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


view model =
    let
        accepted =
            isAccept model.states model.machine.final model.input model.inputAt
    in
        collage 500
            500
            [ text (toString model.states) |> centered |> filled black

            --, text (toString model.input) |> centered |> filled black |> move ( 0, 30 )
            , renderTape model.input model.inputAt |> move ( 0, 30 )
            , text ("inputAt: " ++ toString model.inputAt) |> centered |> filled black |> move ( 0, 60 )
            , text ("Accepted: " ++ toString accepted)
                |> centered
                |> filled
                    (if accepted then
                        green
                     else
                        red
                    )
                |> move ( 0, -30 )
            , group [ roundedRect 30 30 10 |> filled lightGreen, triangle 10 |> filled white ] |> move ( 0, -100 ) |> notifyTap Step
            ]
