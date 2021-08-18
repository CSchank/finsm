module Error exposing (DFAErrorType(..), Error(..), contextHasError, errorIcon, errorMenu, machineCheck)

-- This module serves to export checks and exception handling of finite state machines.
-- When we add support for other machine types, we can extend this module as well.

import Array exposing (Array)
import Dict exposing (Dict)
import Environment exposing (Environment)
import GraphicSVG exposing (..)
import Helpers exposing (..)
import Machine exposing (Machine, MachineType(..), StateID, TransitionID)
import Mistakes exposing (..)
import Set exposing (Set)
import SharedModel exposing (..)
import Tuple exposing (first, second)


type Error
    = NoError
    | DFAError DFAErrorType StateID
    | EpsTransError
    | DuplicateStates (Set StateID)


type DFAErrorType
    = HasEpsilon
    | Incomplete
    | Nondeterministic
    | Unsure -- Good for debugging?


contextHasError : Error -> MachineType -> Bool
contextHasError err mtype =
    case mtype of
        DFA ->
            if err == NoError then
                False

            else
                True

        NFA ->
            case err of
                EpsTransError ->
                    True

                DuplicateStates _ ->
                    True

                _ ->
                    False

        NPDA ->
            case err of
                EpsTransError ->
                    True

                DuplicateStates _ ->
                    True

                _ ->
                    False


machineCheck : SharedModel -> Error
machineCheck sModel =
    let
        mac =
            sModel.machine

        tMistakes =
            getTransitionMistakes mac

        allTransitionLabels =
            List.sort <| Set.toList <| Set.remove "\\epsilon" <| List.foldr Set.union Set.empty <| List.map .inputLabel <| Dict.values mac.transitionNames

        catch : Maybe (Set String) -> List String
        catch ms =
            case ms of
                Nothing ->
                    []

                Just s ->
                    Set.toList s

        getTrans : Dict TransitionID StateID -> List String
        getTrans d =
            (List.concatMap (\e -> Dict.get e mac.transitionNames |> Maybe.map .inputLabel |> catch) <| Dict.keys d) |> List.sort

        foldingFunc : ( StateID, Dict TransitionID StateID ) -> Error -> Error
        foldingFunc sTuple err =
            case err of
                DFAError errType x ->
                    DFAError errType x

                NoError ->
                    let
                        transitions =
                            getTrans <| second sTuple

                        stId =
                            first sTuple
                    in
                    if transitions == allTransitionLabels then
                        NoError

                    else if List.member "\\epsilon" transitions then
                        DFAError HasEpsilon stId

                    else
                        case compare (List.length transitions) (List.length allTransitionLabels) of
                            LT ->
                                DFAError Incomplete stId

                            EQ ->
                                DFAError Incomplete stId

                            -- e.g. compare [1,1,2] [1,2,3], can be Nondeterministic too
                            GT ->
                                DFAError Nondeterministic stId

                otherErr ->
                    otherErr
    in
    if tMistakes /= Nothing then
        EpsTransError

    else
        List.foldr (\x acc -> foldingFunc x acc) NoError <| Dict.toList mac.delta


errorIcon : Color -> Color -> Shape msg
errorIcon backclr shapeclrs =
    group
        [ triangle 20 |> filled backclr |> rotate 22.5
        , roundedRect 7.5 10 5 |> filled shapeclrs |> move ( 0, 7.5 )
        , circle 3 |> filled shapeclrs |> move ( 0, -2.5 )
        ]


errorMenu : Error -> Machine -> Float -> Float -> Shape msg
errorMenu err mac winX winY =
    let
        errStId =
            case err of
                DFAError _ stId ->
                    case Dict.get stId mac.stateNames of
                        Just name ->
                            name

                        Nothing ->
                            ""

                _ ->
                    ""

        errorHeader txt =
            group
                [ errorIcon red white
                , text txt
                    |> size 20
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 20, 0 )
                ]
                |> scale 0.75
                |> move ( 0, -20 )

        errorReason =
            group
                [ circle 3 |> filled red
                , (text <|
                    case err of
                        DFAError HasEpsilon _ ->
                            "Possible cause: There are epsilon transitions"

                        DFAError Incomplete _ ->
                            "Possible cause: There are missing transitions"

                        DFAError Nondeterministic _ ->
                            "Possible cause: There are extraneous transitions"

                        EpsTransError ->
                            "Cause: Epsilon transitions are mixed with normal transitions"

                        _ ->
                            "You might have missed something somewhere?"
                  )
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( 0, -40 )

        errorHint =
            group
                [ circle 3 |> filled red
                , (text <|
                    case err of
                        DFAError HasEpsilon _ ->
                            "Hint: Try removing all your epsilon transitions"

                        DFAError Incomplete _ ->
                            "Hint: Check states for missing transitions"

                        DFAError Nondeterministic _ ->
                            "Hint: Find and remove extra transitions"

                        EpsTransError ->
                            "Hint: Switch to Build mode and fix transitions in red"

                        _ ->
                            ""
                  )
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( 0, -60 )

        errorState =
            group
                [ circle 3 |> filled red
                , text "Hint: Check state "
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                , latex 50 12 "blank" errStId AlignLeft |> move ( 150, 3 )
                ]
                |> move ( 0, -80 )

        actionHint =
            group
                [ circle 3 |> filled red
                , text "Go to Build mode to fix your machine, or use a NFA"
                    |> size 12
                    |> fixedwidth
                    |> filled darkRed
                    |> move ( 15, -5 )
                ]
                |> move ( 0, -100 )
    in
    case err of
        DFAError _ _ ->
            group [ errorHeader "DFA error: Your machine has a problem!", errorReason, errorHint, errorState, actionHint ]

        EpsTransError ->
            group [ errorHeader "Error: You have invalid state transitions!", errorReason, errorHint ]

        NoError ->
            group []

        -- TODO: Add error handling for duplicate state names
        DuplicateStates _ ->
            group []
