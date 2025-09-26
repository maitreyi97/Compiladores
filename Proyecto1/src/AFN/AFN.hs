module AFN.AFN where

import Data.Maybe

data AFN = AFN
  [String] -- Estados
  [Char] -- Alfabeto
  String -- Estado Inicial
  [(String, Char, [String])] -- Transiciones
  [String] -- EstadosFinales
  deriving (Show)

aceptaAFN :: AFN -> String -> Bool
aceptaAFN (AFN estados alfabeto estadoInicial transiciones estadosFinales) cadena =
  or (map (`elem` (transicion transiciones [estadoInicial] cadena)) estadosFinales)

transicion :: [(String, Char, [String])] -> [String] -> String -> [String]
transicion _ estadoActual "" = estadoActual
transicion transiciones estados (x:xs) =
  transicion transiciones (concat (map (\estadoActual -> auxiliarTransicion transiciones estadoActual x) estados)) xs
  
auxiliarTransicion :: [(String, Char, [String])] -> String -> Char -> [String]
auxiliarTransicion [] _ _ = []
auxiliarTransicion ((estadoInicial, caracter, estadoFinal):cs) estadoActual simbolo
  | estadoInicial == estadoActual && simbolo == caracter = estadoFinal ++ auxiliarTransicion cs estadoActual simbolo
  | otherwise = auxiliarTransicion cs estadoActual simbolo


