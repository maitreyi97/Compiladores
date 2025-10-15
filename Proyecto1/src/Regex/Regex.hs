module Regex where

data RegEx = Epsilon 
            | Literal Char
            | Or RegEx RegEx
            | Concat RegEx RegEx
            | Star RegEx
            | Plus RegEx
            deriving Eq

instance Show RegEx where 
    show (Star r) = "(" ++ show r ++ ")*"
    show (Concat r1 r2) = "(" ++ show r1 ++ show r2 ++ ")"
    show (Or r1 r2) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")"
    show Epsilon = "ε"
    show (Literal c) = [c]
    show (Plus r) = "(" ++ show r ++ ")+"