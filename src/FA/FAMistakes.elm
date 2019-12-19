module Mistakes exposing (checkEpsilonTransLabel, checkTransitionValid, getTransitionMistakes)

import Dict exposing (..)
import Machine exposing (..)
import Set exposing (..)


getTransitionMistakes : Machine -> TransitionMistakes
getTransitionMistakes mac =
    let
        tNames =
            mac.transitionNames
    in
    checkEpsilonTransLabel tNames



-- Check if an epsilon label is well-typed


checkEpsilonTransLabel : TransitionNames -> TransitionMistakes
checkEpsilonTransLabel tNames =
    let
        tMistakes =
            Dict.foldl
                (\tid tnames tmistakes ->
                    if not (checkTransitionValid tnames) then
                        Set.insert tid tmistakes

                    else
                        tmistakes
                )
                Set.empty
                tNames
    in
    if Set.isEmpty tMistakes then
        Nothing

    else
        Just tMistakes


checkTransitionValid : Set.Set String -> Bool
checkTransitionValid set =
    case Set.member "\\epsilon" set of
        False ->
            True

        True ->
            if Set.size set == 1 then
                True

            else
                False
