module AFD.AFD where

import Data.Maybe



data AFD = AFD
  [String] -- Estados
  [Char] -- Alfabeto
  String -- Estado Inicial
  [(String, Char, String)] -- Transiciones
  [String] -- Estados Finales
  deriving (Show)

acepta :: AFD -> String -> Bool
acepta (AFD estados alfabeto estadoInicial transiciones estadosFinales) cadena =
  ((transicion transiciones estadoInicial cadena) `elem` estadosFinales)

transicion :: [(String, Char, String)] -> String -> String -> String
transicion _ estado "" = estado
transicion transiciones estado (x:xs) = case (buscarTransicion transiciones estado x) of
  Just estadoNuevo -> transicion transiciones estadoNuevo xs
  Nothing -> "EstadoRechazo"
  

buscarTransicion :: [(String, Char, String)] -> String -> Char -> Maybe String 
buscarTransicion [] _ _ = Nothing
buscarTransicion ((estadoInicial, caracter, estadoFinal):xs) estado simbolo
  | estadoInicial == estado && caracter == simbolo = Just estadoFinal
  | otherwise = buscarTransicion xs estado simbolo
