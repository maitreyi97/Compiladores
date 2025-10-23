module Regex.Regex where

-- Representación de Expresiones Regulares
-- Epsilon: representa la cadena vacía
-- Literal c: representa el carácter c
-- Or r1 r2: representa la disyunción entre las expresiones r1 y r2
-- Concat r1 r2: representa la concatenación de las expresiones r1 y r2
-- Star r: representa la clausura de Kleene de la expresión r
-- Plus r: representa una o más repeticiones de la expresión r
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

