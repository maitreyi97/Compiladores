module Regex.Regex where

data RegEx = Epsilon 
            | Literal Char
            | Or RegEx RegEx
            | Concat RegEx RegEx
            | Star RegEx
            deriving Eq
