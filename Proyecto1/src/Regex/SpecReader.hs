module Regex.SpecReader where

import Regex.Regex
import Regex.RegexParser
import Data.Char (isSpace)
import Data.List.Split (splitOn)

type Categoria = (String, RegEx)

leeSpec :: FilePath -> IO [String]
leeSpec archivo = do
    contenido <- readFile archivo
    return (Prelude.lines contenido)

removerEspacio :: String -> String
removerEspacio = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseLine :: String -> Maybe Categoria
parseLine ('-':'-':cs) = Nothing
parseLine "" = Nothing
parseLine linea = Just (categoria, expresion)
    where
        x = removerEspacio linea
        [c,cs] = splitOn "->" x
        tokens = lexer (removerEspacio cs)
        categoria = removerEspacio c
        expresion = parseRegex tokens

obtenerCategoria :: [String] -> [Categoria]
obtenerCategoria [] = []
obtenerCategoria (x:xs) = 
    case parseLine x of
        Just r -> r : obtenerCategoria xs
        Nothing -> obtenerCategoria xs

prueba :: String -> RegEx
prueba x = 
    let 
        tokens = lexer x 
    in parseRegex tokens