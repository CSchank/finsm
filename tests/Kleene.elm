{-
Here, we develop an implementation of the Kleene Algebra, which is the
underlying generalized structure of regular expressions.
-}

module Kleene exposing (..)

import Regex exposing (Regex, Match, fromString, never, find, replace)
import Maybe 

type alias Algebra a = Kleene a -> a

type Fix = Fix (Kleene Fix)

unFix : Fix -> Kleene Fix
unFix (Fix x) = x

type Kleene a =
    Empty
    | Id
    | Member a
    | Plus (Kleene a) (Kleene a)
    | Mul (Kleene a) (Kleene a)
    | Star (Kleene a)

fmap : (a -> b) -> Kleene a -> Kleene b
fmap f expr =
    case expr of
        Empty -> Empty
        Id -> Id
        Member a -> Member (f a)
        Plus a b -> Plus (fmap f a) (fmap f a)
        Mul a b  -> Mul (fmap f a) (fmap f b)
        Star a -> Star (fmap f a)

eval : Algebra String
eval alg =
    case alg of
        Empty    -> mkundefined "" -- Bottom
        Id       -> ""
        Member a -> a
        Plus a b -> (eval a) ++ "+" ++ (eval b)
        Mul a b  -> (eval a) ++ (eval b)
        Star a   -> (eval a) ++ "*"

cata : Algebra a -> Fix -> a
cata alg = alg << fmap (cata alg) << unFix

well_typed_example = Star (Member "a")

mkundefined : a -> whatever
mkundefined x = mkundefined x