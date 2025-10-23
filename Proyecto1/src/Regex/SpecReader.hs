module Regex.SpecReader where

import Regex.Regex
import Regex.RegexParser
import Data.Char (isSpace)
import Data.List.Split (splitOn)
-- Definición de Categoria
-- Una tupla que continene el nombre de la categoría y su expresión regular asociada.
type Categoria = (String, RegEx)

-- leeSpec
-- Lee un archivo de especificaciones y devuelve una lista de líneas.
leeSpec :: FilePath -> IO [String]
leeSpec archivo = do
    contenido <- readFile archivo
    return (Prelude.lines contenido)

-- removerEspacio
-- Elimina los espacios en blanco al inicio y al final de una cadena.

removerEspacio :: String -> String
removerEspacio = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- parseLine
-- Parsea una línea del archivo de especificaciones.
-- Devuelve Nothing si la línea es un comentario o está vacía.
-- De lo contrario, devuelve Just Categoria.
parseLine :: String -> Maybe Categoria
parseLine ('-':'-':cs) = Nothing
parseLine "" = Nothing
parseLine linea = 
    case splitOn "->" (removerEspacio linea) of
        [c,cs] ->
            let tokens = lexer (removerEspacio cs)
                categoria = removerEspacio c
                expresion = parseRegex tokens
            in Just (categoria, expresion)
        _ -> Nothing

-- obtenerCategoria
-- Dada una lista de líneas del archivo de especificaciones,
-- Devuelve una lista de categorías con sus expresiones regulares correspondientes.
obtenerCategoria :: [String] -> [Categoria]
obtenerCategoria [] = []
obtenerCategoria (x:xs) = 
    case parseLine x of
        Just r -> r : obtenerCategoria xs
        Nothing -> obtenerCategoria xs