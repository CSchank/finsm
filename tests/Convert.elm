module Convert exposing (Intermediate, Level, Queue, char, chars, charsHelper, convertFromSimple, convertListCharRegex0, convertNFA, convertNFA0, convertRegex, convertRegexToNFA, convertStringRegex, convertTransitions, decomposeLevels, defaultCap, escape, flattenPattern, ident, kleeneOp, mapCharWithTID, parseKleene, placeStates, reserved, topParser)

{-
   This module performs conversion on representations of
   regular languages.

   The two primary ones required for a minimal test interface are
   1. Kleene -> JS.Regex
   2. Kleene -> Machine.machine
-}

import Debug exposing (todo)
import Dict exposing (..)
import Kleene exposing (..)
import Machine exposing (..)
import Maybe exposing (..)
import Parser exposing (..)
import Regex exposing (Match, Regex, find, fromString, never, replace)
import Set exposing (..)
import Simple exposing (..)
import String exposing (cons)



-- EXPOSED FUNCTIONS


convertRegex : Kleene String -> Regex
convertRegex =
    eval >> convertStringRegex


convertRegexToNFA : String -> Machine
convertRegexToNFA =
    parseKleene >> convertNFA


convertStringRegex inp =
    convertListCharRegex0 (String.toList inp) ""



-- Observations:
-- 1. The empty string is (badly) represented by the special escape character '\B'
-- 2. We can ignore special characters with a prefixed escape
-- 3. Parenthesis are (badly) represented as non-capturing parenthesis with pattern "(?:x)", x is a variable


convertListCharRegex0 : List Char -> String -> Regex
convertListCharRegex0 cs s =
    case cs of
        [] ->
            withDefault never (fromString <| String.replace "ε" "\\B" <| String.reverse s)

        x :: y :: xs ->
            if x == '\\' then
                convertListCharRegex0 xs <| cons y (cons x s)

            else if x == '(' then
                convertListCharRegex0 (y :: xs) <| cons ':' (cons '?' (cons '(' s))

            else
                convertListCharRegex0 (y :: xs) <| cons x s

        x :: [] ->
            convertListCharRegex0 [] <| cons x s



-----------------------------
-- String to Kleene        --
-----------------------------


parseKleene : String -> Kleene String
parseKleene =
    run topParser >> Result.withDefault Empty


topParser : Parser (Kleene String)
topParser =
    oneOf
        [ backtrackable <| lazy (\_ -> kleeneOp topParser)
        , ident
        , chars

        -- , escape
        -- , paren (\_ -> topParser)
        ]


kleeneOp : Parser (Kleene String) -> Parser (Kleene String)
kleeneOp rec =
    succeed
        (\a ( op, b ) ->
            case op of
                "|" ->
                    Plus a b

                "^" ->
                    Mul a b

                "*" ->
                    Star a

                _ ->
                    Empty
        )
        |. symbol "("
        |= lazy (\_ -> rec)
        |= oneOf
            [ succeed (\op -> ( op, Empty ))
                |. symbol ")"
                |= (getChompedString <| symbol "*")
            , succeed (\op b -> ( op, b ))
                |= (getChompedString <| oneOf [ symbol "|", symbol "^" ])
                |= lazy (\_ -> rec)
                |. symbol ")"
            ]


reserved =
    [ '|', '*', 'ε', '(', ')', '\\' ]


chars : Parser (Kleene String)
chars =
    loop "" charsHelper


charsHelper : String -> Parser (Step String (Kleene String))
charsHelper str =
    oneOf
        [ succeed (\c -> Loop (str ++ c))
            |= oneOf [ char, escape ]
        , succeed <| Done (Alpha str)
        ]


char : Parser String
char =
    getChompedString <| chompIf Char.isAlphaNum


escape : Parser String
escape =
    getChompedString <|
        chompIf (\c -> c == '\\')
            |. chompIf (\c -> Set.member c special)


ident : Parser (Kleene String)
ident =
    succeed Id
        |. (getChompedString <| chompIf (\c -> c == 'ε'))



{-
   paren : Parser a -> Parser a
   paren parser =
       succeed ()
         |= symbol '('
         |= parser
         |= symbol ')'
-}
-----------------------------
-- Kleene to Machines      --
-----------------------------


defaultCap : Capacity
defaultCap =
    10000


convertNFA : Kleene String -> Machine
convertNFA ks =
    convertNFA0 ks 0
        |> Maybe.withDefault (identityMachine 0)
        |> Tuple.first
        |> convertFromSimple


convertNFA0 : Kleene String -> Capacity -> Maybe ( SimpleMachine, Capacity )
convertNFA0 expr tank =
    if tank > defaultCap then
        Nothing

    else
        case expr of
            Empty ->
                Just <| emptyMachine tank

            Id ->
                Just <| identityMachine tank

            Alpha str ->
                Just <| catConstruct str tank

            Plus a b ->
                let
                    e1 =
                        convertNFA0 a tank
                in
                case e1 of
                    Nothing ->
                        Nothing

                    Just ( m1, tank1 ) ->
                        let
                            e2 =
                                convertNFA0 b tank1
                        in
                        case e2 of
                            Nothing ->
                                Nothing

                            Just ( m2, tank2 ) ->
                                Just <| plusConstruct m1 m2 tank2

            Mul a b ->
                let
                    e1 =
                        convertNFA0 a tank
                in
                case e1 of
                    Nothing ->
                        Nothing

                    Just ( m1, tank1 ) ->
                        let
                            e2 =
                                convertNFA0 b tank1
                        in
                        case e2 of
                            Nothing ->
                                Nothing

                            Just ( m2, tank2 ) ->
                                Just <| mulConstruct m1 m2 tank2

            Star a ->
                let
                    e =
                        convertNFA0 a tank
                in
                case e of
                    Nothing ->
                        Nothing

                    Just ( m, tank1 ) ->
                        Just <| asteriskConstruct m tank1



------------------------------
-- SimpleMachine to Machine --
------------------------------


type alias Level =
    Int


type alias Queue =
    List ( Level, StateID )


type alias Intermediate =
    { stateTransitions : StateTransitions -- Dict ( StateID, TransitionID, StateID ) ( Float, Float )
    , stateNames : StateNames -- Dict StateID String
    , levelNodes : Dict Level (Set StateID)
    }


convertFromSimple : SimpleMachine -> Machine
convertFromSimple simple =
    let
        ( delta, tNames ) =
            convertTransitions simple.transitions

        decompose =
            decomposeLevels Dict.empty
                (List.singleton ( 0, Maybe.withDefault 0 <| List.head <| Set.toList simple.s ))
                delta
                { stateTransitions = Dict.empty, stateNames = Dict.empty, levelNodes = Dict.empty }

        statePos =
            placeStates decompose.levelNodes
    in
    { q = simple.q
    , delta = delta
    , start = simple.s
    , final = simple.f
    , statePositions = statePos
    , stateTransitions = decompose.stateTransitions
    , stateNames = decompose.stateNames
    , transitionNames = tNames
    , transitionMistakes = Nothing
    }


placeStates : Dict Level (Set StateID) -> StatePositions
placeStates dict =
    let
        minY =
            -250

        maxY =
            250

        minX =
            -500

        maxX =
            500

        -- maxWidth = Dict.foldr (\_ v curmax -> max curmax <| Set.size v) 0 dict
        numLevels =
            Dict.size dict

        transformY =
            Dict.map
                (\_ set ->
                    Set.toList set
                        |> List.indexedMap (\idx stId -> ( stId, toFloat minY + toFloat ((idx + 1) * (maxY - minY)) / toFloat (Set.size set + 1) ))
                )
                dict

        transformX =
            Dict.toList transformY
                |> List.concatMap
                    (\( lvl, lst ) ->
                        List.map (\( stId, flt ) -> ( stId, ( toFloat minX + toFloat ((lvl + 1) * (maxX - minX)) / toFloat (Dict.size dict + 1), flt ) )) lst
                    )
                |> Dict.fromList
    in
    transformX


decomposeLevels :
    Dict StateID Level
    -> Queue
    -> Delta
    -> Intermediate
    -> Intermediate
decomposeLevels visited unvisited pre post =
    case unvisited of
        [] ->
            post

        ( level, stId ) :: rest ->
            if Dict.member stId visited then
                decomposeLevels visited rest pre post

            else
                let
                    newVisited =
                        Dict.insert stId level visited

                    nextLevel =
                        level + 1

                    newUnvisited =
                        rest ++ List.map (\x -> ( nextLevel, x )) (Dict.values patterns)

                    patterns =
                        Maybe.withDefault Dict.empty (Dict.get stId pre)

                    transitions =
                        Dict.toList patterns

                    newStateNames =
                        Dict.insert stId ("q_{" ++ String.fromInt stId ++ "}") post.stateNames

                    insertTPos : TransitionID -> StateID -> ( ( StateID, TransitionID, StateID ), ( Float, Float ) )
                    insertTPos tID0 stID0 =
                        case Dict.get stID0 visited of
                            Nothing ->
                                ( ( stId, tID0, stID0 ), ( 0, 0 ) )

                            Just lvl0 ->
                                if lvl0 < level then
                                    ( ( stId, tID0, stID0 ), ( 0, toFloat <| (level - lvl0) * 10 ) )

                                else
                                    ( ( stId, tID0, stID0 ), ( 0, 0 ) )

                    newStateTransitions =
                        List.foldr
                            (\( tId1, sId1 ) d ->
                                let
                                    ( arrow, pos ) =
                                        insertTPos tId1 sId1
                                in
                                Dict.insert arrow pos d
                            )
                            post.stateTransitions
                            transitions

                    getCurLevelNodes =
                        Dict.get level post.levelNodes

                    newLevelNodes =
                        case getCurLevelNodes of
                            Nothing ->
                                Dict.insert level (Set.singleton stId) post.levelNodes

                            Just setLevel ->
                                Dict.insert level (Set.insert stId setLevel) post.levelNodes

                    newIntermediate =
                        { stateTransitions = newStateTransitions
                        , stateNames = newStateNames
                        , levelNodes = newLevelNodes
                        }
                in
                decomposeLevels newVisited newUnvisited pre newIntermediate


convertTransitions : Transitions -> ( Delta, TransitionNames )
convertTransitions t =
    let
        flattenVal =
            Dict.map (\_ v -> flattenPattern v) t

        ( outDelta, outTNames, _ ) =
            Dict.foldr
                (\k v ( delta, tNames, curTID ) ->
                    let
                        ( deltaInner, tNamesNew, nextTID ) =
                            mapCharWithTID curTID v
                    in
                    ( Dict.insert k deltaInner delta, Dict.union tNamesNew tNames, nextTID )
                )
                ( Dict.empty, Dict.empty, 0 )
                flattenVal
    in
    ( outDelta, outTNames )


flattenPattern : Dict Character (Set StateID) -> List ( Character, StateID )
flattenPattern chDict =
    let
        chList =
            Dict.toList chDict

        flattenedList =
            List.concatMap
                (\( ch, setStId ) ->
                    List.map (\stId -> ( ch, stId )) <| Set.toList setStId
                )
                chList
    in
    flattenedList


mapCharWithTID : TransitionID -> List ( Character, StateID ) -> ( Dict TransitionID StateID, TransitionNames, TransitionID )
mapCharWithTID start assocCharStId =
    let
        rangeEnd =
            start + List.length assocCharStId - 1

        idRange =
            List.range start (start + rangeEnd)

        ( listChar, listStId ) =
            List.unzip assocCharStId

        tNames =
            unsafeZip idRange <| List.map (\str -> Set.singleton str) listChar

        deltaVal =
            unsafeZip idRange listStId
    in
    ( Dict.fromList deltaVal, Dict.fromList tNames, rangeEnd + 1 )
