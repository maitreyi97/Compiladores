module Regex.Regex where

data RegEx = Epsilon 
            | Literal Char
            | Or RegEx RegEx
            | Concat RegEx RegEx
            | Star RegEx
            deriving Eq

instance Show RegEx where 
    show (Star r) = "(" ++ show r ++ ")*"
    show (Concat r1 r2) = "(" ++ show r1 ++ show r2 ++ ")"
    show (Or r1 r2) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")"
    show Epsilon = "ε"
    show (Literal c) = [c]


matchRegEx :: RegEx -> String -> Bool
matchRegEx Epsilon c = c == ""
matchRegEx (Literal x) c = c == [x]
matchRegEx (Or i d) c = matchRegEx i c || matchRegEx d c
matchRegEx (Concat i d) (c:cs) = matchRegEx i [c] && matchRegEx d cs
matchRegEx (Star r) c = True
    where
        splits = allSplits c 

        allSplits :: String -> [(String, String)]
        allSplits st = [splitAt i st | i <- [0..length st]]

