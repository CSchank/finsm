module Kleene exposing (Kleene(..), eval, fmap, mkundefined, sanitize, special, well_typed_example)

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
        Empty ->
            mkundefined ""

        -- Bottom
        Id ->
            "ε"

        Alpha a ->
            sanitize a

        Plus a b ->
            eval a ++ "|" ++ eval b

        Mul a b ->
            eval a ++ eval b

        Star a ->
            "(" ++ eval a ++ ")*"


well_typed_example =
    Plus Id (Alpha ":")


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
