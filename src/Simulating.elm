module Simulating exposing (Character, InputTape, PersistentModel, initPersistentModel, latexKeyboard, view)

import Array exposing (Array)
import Dict exposing (Dict)
import Machine exposing (..)
import Set exposing (Set)


type alias PersistentModel =
    { tapes : Dict Int (Array Character)
    , currentStates : Set StateID
    }


type alias InputTape =
    Array Character


type alias Character =
    String


initPersistentModel : PersistentModel
initPersistentModel =
    { tapes =
        Dict.fromList
            [ ( 0, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0" ] )
            , ( 1, Array.fromList [ "0", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0" ] )
            ]
    , currentStates = test.start
    }


view : Environment -> ( Model, PersistentModel, SharedModel ) -> Shape Msg
view env ( model, pModel, sModel ) =
    let
        oldMachine =
            model.machine sModel.machine

        winX =
            toFloat <| first model.environment.windowSize

        winY =
            toFloat <| second model.environment.windowSize

        chars =
            Set.toList <| Set.fromList <| List.map (\( _, n ) -> n) <| Dict.toList oldMachine.transitionNames

        getStateName sId =
            case Dict.get sId oldMachine.stateNames of
                Just n ->
                    n

                Nothing ->
                    "\\ "

        menu =
            group
                [ text "Simulate"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 2, winY / 6 - 15 )
                , text "(Click to toggle start state(s), right arrow to scrub through tape)"
                    |> size 6
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 85, winY / 6 - 15 )
                , group
                    [ roundedRect 15 15 2
                        |> filled white
                        |> addOutline (solid 1) darkGray
                    , text "+"
                        |> size 16
                        |> fixedwidth
                        |> filled black
                        |> move ( -4.5, -5 )
                        |> notifyTap (SMsg AddNewTape)
                    ]
                    |> move ( -winX / 2 + 20, winY / 6 - 35 - 25 * (toFloat <| Dict.size model.simulateData.tapes) )
                , text "Machine"
                    |> size 16
                    |> fixedwidth
                    |> filled black
                    |> move ( -winX / 2 + 492, winY / 6 - 15 )
                , latex 500 18 "let\\ N = (Q,\\Sigma,\\Delta,S,F)" AlignLeft
                    |> move ( -winX / 2 + 750, winY / 6 - 25 )
                , latex 500 14 "where" AlignLeft
                    |> move ( -winX / 2 + 750, winY / 6 - 45 )
                , latex 500 18 ("Q = \\{ " ++ String.join "," (Dict.values oldMachine.stateNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 65 )
                , latex 500 18 ("\\Sigma = \\{ " ++ String.join "," (Set.toList <| Set.fromList <| Dict.values oldMachine.transitionNames) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 90 )
                , latex 500 18 "\\Delta = (above)" AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 115 )
                , latex 500 18 ("S = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| oldMachine.start) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 140 )
                , latex 500 18 ("F = \\{ " ++ String.join "," (List.map getStateName <| Set.toList <| oldMachine.final) ++ " \\}") AlignLeft
                    |> move ( -winX / 2 + 760, winY / 6 - 165 )
                , case model.appState of
                    Simulating (SimRegular tapeId charId) ->
                        group (List.indexedMap (\x ( chId, ch ) -> renderTape ch chId tapeId charId True |> move ( 0, -(toFloat x) * 25 )) <| Dict.toList tapes)
                            |> move ( -winX / 2 + 20, winY / 6 - 40 )

                    _ ->
                        group []
                ]

        tapes =
            model.simulateData.tapes
    in
    group
        [ case model.appState of
            Simulating (SimRegular _ _) ->
                group
                    [ rect winX (winY / 3)
                        |> filled lightGray
                    , menu
                    ]
                    |> move ( 0, -winY / 3 )

            Simulating (SimEditing tapeId) ->
                let
                    tape =
                        case Dict.get tapeId model.simulateData.tapes of
                            Just t ->
                                t

                            Nothing ->
                                Array.empty
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
                    , renderTape tape tapeId -1 -1 False
                        |> move ( -10 * toFloat (Array.length tape), winY / 6 - 65 )
                    ]
                    |> move ( 0, -winY / 3 )

            _ ->
                group []
        ]


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
                , latex (keyW / 1.5) (keyH / 1.5) char AlignCentre
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
