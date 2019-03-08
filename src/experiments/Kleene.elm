module Kleene exposing (Kleene(..), eval, fmap, mkundefined, mul, sanitize, special, well_typed_example)

{-
   Here, we develop an implementation of the Kleene Algebra, which is the
   underlying generalized structure of regular expressions.
-}

import Set exposing (fromList, member)
import String exposing (cons, foldr)


type Kleene a
    = Empty
    | Id
    | Alpha a
    | Plus (Kleene a) (Kleene a)
    | Mul (Kleene a) (Kleene a)
    | Star (Kleene a)


fmap : (a -> b) -> Kleene a -> Kleene b
fmap f expr =
    case expr of
        Empty ->
            Empty

        Id ->
            Id

        Alpha a ->
            Alpha (f a)

        Plus a b ->
            Plus (fmap f a) (fmap f a)

        Mul a b ->
            Mul (fmap f a) (fmap f b)

        Star a ->
            Star (fmap f a)


eval : Kleene String -> String
eval expr =
    case expr of
        -- Bottom
        Empty ->
            mkundefined ""

        Id ->
            "ε"

        Alpha a ->
            sanitize a

        Plus a b ->
            eval a ++ "|" ++ eval b

        Mul a b ->
            let
                sa =
                    eval a

                sb =
                    eval b

                strA =
                    if unifiable a then
                        sa

                    else
                        "(" ++ sa ++ ")"

                strB =
                    if unifiable b then
                        sb

                    else
                        "(" ++ sb ++ ")"
            in
            strA ++ strB

        Star a ->
            if unifiable expr then
                eval a ++ "*"

            else
                "(" ++ eval a ++ ")*"


well_typed_example =
    Plus Id (Alpha ":")


unifiable : Kleene String -> Bool
unifiable a =
    case a of
        Empty ->
            False

        Id ->
            True

        Alpha x ->
            True

        Plus _ _ ->
            False

        Mul x y ->
            unifiable x && unifiable y

        Star (Alpha x) ->
            String.length x == 1

        _ ->
            False


mkundefined : a -> whatever
mkundefined x =
    mkundefined x



-- Special characters: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions


special =
    fromList
        [ '\\'
        , '^'
        , '$'
        , '*'
        , '+'
        , '?'
        , '.'
        , '('
        , ')'
        , ':'
        , '='
        , '!'
        , '<'
        , '{'
        , '}'
        , '['
        , ']'
        , ','
        , '|'
        ]


sanitize : String -> String
sanitize s =
    foldr
        (\c str ->
            if member c special then
                cons '\\' (cons c str)

            else
                cons c str
        )
        ""
        s



-- Smart constructor of Mul. Eliminates unneccessary empty strings,
-- as well as obeying precedence rules.
-- This should only be used with parsing probably.


mul : Kleene String -> Kleene String -> Kleene String
mul a b =
    case b of
        Empty ->
            Empty

        Id ->
            a

        Alpha "" ->
            a

        Alpha str ->
            case a of
                Empty ->
                    Empty

                Id ->
                    b

                Alpha "" ->
                    b

                Alpha al ->
                    Alpha (al ++ str)

                _ ->
                    Mul a b

        Plus c d ->
            Plus (mul a c) d

        Mul c d ->
            case a of
                Empty ->
                    Empty

                Id ->
                    b

                Alpha "" ->
                    b

                _ ->
                    Mul a b

        Star c ->
            case a of
                Empty ->
                    Empty

                Id ->
                    b

                Alpha "" ->
                    b

                _ ->
                    Mul a b
