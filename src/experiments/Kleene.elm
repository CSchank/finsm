module Kleene exposing (..)

{-
   Here, we develop an implementation of the Kleene Algebra, which is the
   underlying generalized structure of regular expressions.
-}

import Debug exposing (todo)
import Set exposing (Set,fromList, member)
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
            "!"

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

plus : Kleene a -> Kleene a -> Kleene a
plus a b =
    case b of
        Empty  -> a
        _     -> plus_idempotent a b

star : Kleene a -> Kleene a
star a =
    case a of
        Empty -> Id
        Id    -> Id
        _     -> Star a

foldplus : Kleene a -> List (Kleene a) -> Kleene a
foldplus empty xs = 
    if List.length xs == 0
        then empty
    else
        List.foldr Plus (Maybe.withDefault empty (List.head xs)) 
            <| Maybe.withDefault [] <| List.tail xs

plus_idempotent : Kleene a -> Kleene a -> Kleene a
plus_idempotent a b = 
    if a == b
        then a
    else
        Plus a b

zero_elim : Kleene a -> Kleene a
zero_elim a =
    case a of
        Empty -> Empty
        Id    -> Id
        Alpha x -> Alpha x
        Plus x y ->
            let
                elim_x = zero_elim x
                elim_y = zero_elim y
            in
                if elim_x == Empty
                    then elim_y
                else if elim_y == Empty
                    then elim_x
                else
                    Plus elim_x elim_y
        Mul x y ->
            let
                elim_x = zero_elim x
                elim_y = zero_elim y
            in
                if elim_x == Empty || elim_y == Empty
                    then Empty
                else
                    Mul elim_x elim_y
        Star x ->
            case x of
                Empty -> Id
                Id    -> Id
                _     -> star <| zero_elim x

one_elim : Kleene a -> Kleene a
one_elim a =
    case a of
        Plus x y ->
            let
                elim_x = one_elim x
                elim_y = one_elim y
            in
                plus_idempotent elim_x elim_y
        Mul x y ->
            let
                elim_x = one_elim x
                elim_y = one_elim y
            in
                if elim_x == Id
                    then elim_y
                else if elim_y == Id
                    then elim_x
                else Mul elim_x elim_y
        Star x -> Star <| one_elim x
        _ -> a

