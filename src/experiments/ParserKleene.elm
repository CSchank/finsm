module ParserKleene exposing (branch, char, chars, charsHelper, empty, endInput, escape, flip, ident, initLast, initLastHelper, kleeneOp, oldParser, paren, parseKleene, reserved, topParser)

import Debug exposing (todo)
import Kleene exposing (..)
import List exposing (..)
import Parser exposing (..)
import Set exposing (..)
import Tuple exposing (first, mapBoth, mapFirst, second)


parseKleene : String -> Kleene String
parseKleene =
    run topParser >> Result.withDefault Empty


topParser : Parser (Kleene String)
topParser =
    succeed (|>)
        |= oneOf [ paren, ident, chars ]
        |= branch


branch : Parser (Kleene String -> Kleene String)
branch =
    oneOf
        [ succeed (\expr -> flip Plus expr)
            |. token "|"
            |= lazy (\_ -> topParser)
        , backtrackable <| succeed (\expr -> flip mul expr)
            |. oneOf [ token "^", token "" ]
            |= paren
        , succeed (\expr -> flip mul expr)
            |. token "^"
            |= lazy (\_ -> topParser)
        , succeed (\expr -> flip mul expr)
            |= lazy (\_ -> topParser)
        , succeed identity
        ]


paren : Parser (Kleene String)
paren =
    succeed (|>)
        |. token "("
        |= lazy (\_ -> topParser)
        |. token ")"
        |= oneOf
            [ succeed (\op prev -> op (Star prev))
                |. token "*"
                |= lazy (\_ -> branch)
            , succeed (\expr -> flip Mul expr)
                |= lazy (\_ -> topParser)
            , succeed identity
            ]


chars : Parser (Kleene String)
chars =
    loop "" charsHelper


charsHelper : String -> Parser (Step String (Kleene String))
charsHelper str =
    oneOf
        [ succeed
            (\c ->
                if c == "*" then
                    Done (initLast str |> mapBoth Alpha (Alpha >> Star) |> (\( x, y ) -> mul x y))

                else
                    Loop (str ++ c)
            )
            |= oneOf [ char, escape ]
        , case str of
            "" ->
                problem "Empty!"

            _ ->
                succeed <| Done <| Alpha str
        ]


char : Parser String
char =
    getChompedString <| chompIf <| \x -> Char.isAlphaNum x || x == '*'


escape : Parser String
escape =
    succeed identity
        |. chompIf (\c -> c == '\\')
        |= getChompedString (chompIf (\c -> Set.member c special))


ident : Parser (Kleene String)
ident =
    succeed Id
        |. (getChompedString <| chompIf (\c -> c == 'ε'))


empty : Parser (Kleene String)
empty =
    succeed Empty


endInput : Parser (Kleene String)
endInput =
    succeed Id |. end



-- split a string into its init and last


initLast : String -> ( String, String )
initLast s =
    initLastHelper (String.toList s) |> mapBoth String.fromList String.fromList


initLastHelper : List Char -> ( List Char, List Char )
initLastHelper lc =
    case lc of
        [] ->
            ( [], [] )

        x :: [] ->
            ( [], [ x ] )

        x :: xs ->
            mapFirst ((::) x) <| initLastHelper xs


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


reserved =
    [ '|', '*', 'ε', '(', ')', '\\' ]



-----
-- Old parser definitions live here. They can be removed
-- if the new parser is sufficiently as good.
-----


oldParser : Parser (Kleene String)
oldParser =
    oneOf
        [ backtrackable <| lazy (\_ -> kleeneOp topParser)
        , ident
        , chars
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
