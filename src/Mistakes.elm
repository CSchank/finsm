module Mistakes exposing (..)

import Machine exposing (..)        
import Set exposing (..)
import Dict exposing (..)

getTransitionMistakes : Machine -> TransitionMistakes
getTransitionMistakes mac =
    let
        tNames = mac.transitionNames
    in
        checkEpsilonTransLabel tNames
        

tMistakeRemove : TransitionID -> TransitionMistakes -> TransitionMistakes
tMistakeRemove tId tMistake =
    case tMistake of
        Just setOfMistakes ->
            let
                newSetOfMistakes =
                    Set.remove tId setOfMistakes
            in
            if Set.isEmpty newSetOfMistakes then
                Nothing

            else
                Just newSetOfMistakes

        Nothing ->
            Nothing


tMistakeAdd : TransitionID -> TransitionMistakes -> TransitionMistakes
tMistakeAdd tId tMistake =
    case tMistake of
        Nothing ->
            Just <| Set.singleton tId

        Just setOfMistakes ->
            Just <| Set.insert tId setOfMistakes

{-               
newTransitionMistakes =
    if isValidTransition then
        case oldTransitionMistakes of
            Just setOfMistakes ->
                let
                    newSetOfMistakes =
                        Set.remove newTransID setOfMistakes
                in
                    if Set.isEmpty newSetOfMistakes then
                        Nothing
                            
                    else
                        Just newSetOfMistakes
                            
                            Nothing ->
                                Nothing
                                    
    else
        case oldTransitionMistakes of
            Just setOfMistakes ->
                Just <| Set.insert newTransID setOfMistakes
                    
                    Nothing ->
                        Just <| Set.singleton newTransID
-}                          
                
-- Check if an epsilon label is well-typed                
checkEpsilonTransLabel : TransitionNames -> TransitionMistakes
checkEpsilonTransLabel tNames =
    let
        tMistakes =
            Dict.foldl
                (\tid tnames tmistakes -> if not (checkTransitionValid tnames)
                                          then Set.insert tid tmistakes
                                          else tmistakes)
                Set.empty tNames
    in
        if Set.isEmpty tMistakes
            then Nothing
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

                    
