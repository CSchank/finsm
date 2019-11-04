module Simple exposing (..)

import Debug exposing (..)
import Dict exposing (..)
import List exposing (..)
import Machine exposing (Character, StateID)
import Set exposing (..)



{-
   The purpose of this module is to expose an intermediary machine
   that only exists for the purpose of converting to-from regular
   expressions and the full-fledged machine type living in the main
   app. This allows a closer mathematical correspondence, and we do
   not have to worry about state placements in the definition.
-}


type alias Transitions =
    Dict StateID Pattern


type alias Pattern =
    Dict Character (Set StateID)


type alias SimpleMachine =
    { q : Set StateID
    , s : Set StateID
    , f : Set StateID
    , transitions : Transitions
    }


type alias Capacity =
    Int


emptyMachine : Capacity -> ( SimpleMachine, Capacity )
emptyMachine i =
    let
        empty =
            { q = Set.empty
            , s = Set.empty
            , f = Set.empty
            , transitions = Dict.empty
            }
    in
    ( empty, i )


identityMachine : Capacity -> ( SimpleMachine, Capacity )
identityMachine i =
    catConstruct "" i


catConstruct : String -> Capacity -> ( SimpleMachine, Capacity )
catConstruct s cap =
    let
        startState =
            cap

        endState =
            cap + String.length s

        setStates =
            Set.fromList listCapRange

        listChar =
            String.toList s

        listCapRange =
            List.range startState endState

        constructTransRel : Transitions
        constructTransRel =
            List.foldl (\( char, id ) dict -> Dict.insert id (Dict.singleton (String.fromChar char) (Set.singleton <| id + 1)) dict)
                Dict.empty
            <|
                unsafeZip listChar listCapRange
    in
    ( { q = setStates
      , s = Set.singleton startState
      , f = Set.singleton endState
      , transitions = constructTransRel
      }
    , endState + 1
    )


mulConstruct : SimpleMachine -> SimpleMachine -> Capacity -> ( SimpleMachine, Capacity )
mulConstruct m1 m2 cap =
    ( { q = Set.union m1.q m2.q
      , s = m1.s
      , f = m2.f
      , transitions = addEpsTransSet m1.f m2.s <| Dict.union m1.transitions m2.transitions
      }
    , cap
    )


plusConstruct : SimpleMachine -> SimpleMachine -> Capacity -> ( SimpleMachine, Capacity )
plusConstruct m1 m2 cap =
    let
        newStart =
            Set.singleton cap

        newEnd =
            Set.singleton (cap + 1)

        newStates =
            List.foldr (\small s -> Set.union s small) Set.empty [ m1.q, m2.q, newStart, newEnd ]
    in
    ( { q = newStates
      , s = newStart
      , f = newEnd
      , transitions =
            Dict.union m1.transitions m2.transitions
                |> addInEps cap (Set.union m1.s m2.s)
                |> addOutEps (cap + 1) (Set.union m1.f m2.f)
      }
    , cap + 2
    )


asteriskConstruct : SimpleMachine -> Capacity -> ( SimpleMachine, Capacity )
asteriskConstruct m cap =
    let
        newStartEnd =
            Set.singleton cap

        newStates =
            Set.insert cap m.q
    in
    ( { q = newStates
      , s = newStartEnd
      , f = newStartEnd
      , transitions =
            m.transitions
                |> addInEps cap m.s
                |> addOutEps cap m.f
      }
    , cap + 1
    )



-- Helpers


createEpsTransTo : Set StateID -> Pattern
createEpsTransTo setStates =
    let
        createdPattern =
            List.foldr (\stId set -> Set.insert stId set) Set.empty <| Set.toList setStates
    in
    Dict.singleton "\\epsilon" createdPattern


addInEps : Capacity -> Set StateID -> Transitions -> Transitions
addInEps cap setStates mainDict =
    Dict.insert cap (createEpsTransTo setStates) mainDict


addOutPat : Int -> Maybe Pattern -> Maybe Pattern
addOutPat tgt d =
    Just <|
        case d of
            Nothing ->
                Dict.singleton "\\epsilon" (Set.singleton tgt)

            Just dPrime ->
                case Dict.get "\\epsilon" dPrime of
                    Nothing ->
                        Dict.insert "\\epsilon" (Set.singleton tgt) dPrime

                    Just set ->
                        Dict.insert "\\epsilon" (Set.insert tgt set) dPrime


addOutEps : Capacity -> Set StateID -> Transitions -> Transitions
addOutEps cap setStates mainDict =
    List.foldr (\stId dict -> Dict.update stId (addOutPat cap) dict) mainDict <| Set.toList setStates


addEpsTrans : StateID -> Set StateID -> Transitions -> Transitions
addEpsTrans stId set d =
    case Dict.get stId d of
        Nothing ->
            Dict.insert stId (Dict.singleton "\\epsilon" set) d

        Just subDict ->
            case Dict.get "\\epsilon" subDict of
                Nothing ->
                    Dict.insert stId (Dict.insert "\\epsilon" set subDict) d

                Just setOut ->
                    Dict.insert stId (Dict.insert "\\epsilon" (Set.union setOut set) subDict) d


addEpsTransSet : Set StateID -> Set StateID -> Transitions -> Transitions
addEpsTransSet setStId set d =
    List.foldr (\stId dict -> addEpsTrans stId set dict) d <| Set.toList setStId


unsafeZip : List a -> List b -> List ( a, b )
unsafeZip xs ys =
    case xs of
        [] ->
            []

        x :: restx ->
            case ys of
                [] ->
                    []

                y :: resty ->
                    ( x, y ) :: unsafeZip restx resty

-----------------

getAlphas : SimpleMachine -> Set Character
getAlphas sm = sm.transitions |> Dict.values |> List.map Dict.keys |> List.concat |> Set.fromList

-- Inserts a new transition between two existing states, without overwriting or
-- creation of new states
insertTransitionNoMod : Character -> StateID -> StateID -> SimpleMachine -> SimpleMachine
insertTransitionNoMod char src tgt mac =
    let
        isExistSrc = Dict.member src mac.transitions
        isExistTgt = Dict.member tgt mac.transitions
    in
    if not (isExistSrc && isExistTgt)
        then mac
        else
            let
                innerUpdate mv =
                    case mv of
                        Nothing -> Just <| Set.singleton tgt
                        Just set -> Just <| Set.insert tgt set
                updateFunc mv =
                    case mv of
                        Nothing -> Just (Dict.singleton char <| Set.singleton tgt)
                        Just pat -> Just <| Dict.update char innerUpdate pat
            in { mac | transitions = Dict.update src updateFunc mac.transitions }

-- Delete all outgoing transitions containing the specified character of a state
deleteOutgoingTrans : Character -> StateID -> SimpleMachine -> SimpleMachine
deleteOutgoingTrans char state mac =
    let
        updateFunc mv =
            case mv of
                Nothing -> Nothing
                Just pat -> Just <| Dict.remove char pat
    in
        { mac | transitions = Dict.update state updateFunc mac.transitions }

extendStartStates : Set StateID -> SimpleMachine -> SimpleMachine
extendStartStates setStates mac =
    { mac | s = Set.union mac.s setStates }

-- Get all states reachable from epsilon transitions on start states
epsTransReachable : SimpleMachine -> Set StateID
epsTransReachable mac = todo "todo"



reachable : Transitions -> StateID -> Dict Character (Set StateID)
reachable t st =
    let
        src = Maybe.withDefault Dict.empty <| Dict.get st t
        epsTrans = Maybe.withDefault Set.empty <| Dict.get "\\epsilon" src
        rest = Set.toList epsTrans
                    |> List.map (\trg -> reachable t trg) 
                    |> List.foldr Dict.union Dict.empty
    in
        Dict.union src rest