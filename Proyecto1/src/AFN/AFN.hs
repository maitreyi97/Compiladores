module AFN.AFN where

import Data.List (nub)
import AFD.AFD

data AFN = AFN
  [String] -- Estados
  [Char] -- Alfabeto
  String -- Estado Inicial
  [(String, Char, [String])] -- Transiciones
  [String] -- EstadosFinales
  deriving (Show)

aceptaAFN :: AFN -> String -> Bool
aceptaAFN (AFN estados alfabeto estadoInicial transiciones estadosFinales) cadena =
  or (map (`elem` (transicionAFN transiciones [estadoInicial] cadena)) estadosFinales)

transicionAFN :: [(String, Char, [String])] -> [String] -> String -> [String]
transicionAFN _ estadoActual "" = estadoActual
transicionAFN transiciones estados (x:xs) =
  transicionAFN transiciones (concat (map (\estadoActual -> auxiliarTransicion transiciones estadoActual x) estados)) xs
  
auxiliarTransicion :: [(String, Char, [String])] -> String -> Char -> [String]
auxiliarTransicion [] _ _ = []
auxiliarTransicion ((estadoInicial, caracter, estadoFinal):cs) estadoActual simbolo
  | estadoInicial == estadoActual && simbolo == caracter = estadoFinal ++ auxiliarTransicion cs estadoActual simbolo
  | otherwise = auxiliarTransicion cs estadoActual simbolo


afn_afd :: AFN -> AFD
afn_afd (AFN estados alfabeto estadoInicial transiciones estadosFinales) =
  let potenciaEstados = conjuntoPotencia estados
      tablaTransiciones = transicionesPotencia transiciones potenciaEstados alfabeto
      estadosFactibles = estadosAlcanzables tablaTransiciones [[estadoInicial]] [[estadoInicial]]
      nuevosEstadosFinales = (filter (\x -> (or (map (\y -> (y `elem` x)) estadosFinales ))) estadosFactibles)
      transicionesFinales = filter (\(x,_,_) -> x `elem` estadosFactibles) tablaTransiciones
      estadosFactiblesFormateados = (map (\s -> (concat s)) estadosFactibles)
      transicionesFinalesFormateadas = (map (\(s0,c, s1) -> ((concat s0), c, (concat s1) )) transicionesFinales)
      nuevosEstadosFinalesFormateados  = (map (\s -> (concat s)) nuevosEstadosFinales)
      in (AFD estadosFactiblesFormateados alfabeto estadoInicial transicionesFinalesFormateadas nuevosEstadosFinalesFormateados)



-- estadosAlcanzables :: Transiciones -> EstadosActuales -> EstadosVistos -> EstadosAlcanzables
estadosAlcanzables :: [([String], Char, [String])] -> [[String]] -> [[String]] -> [[String]]
estadosAlcanzables _ [] alcanzados = alcanzados
estadosAlcanzables transiciones estadosActuales alcanzados =
  let estadosSiguientes = concat (nub (map (\q -> estadoAlcanzableAux transiciones q) estadosActuales))
      estadosNuevos = filter (\q -> (not (null q) && (not (q `elem` alcanzados)))) estadosSiguientes
  in estadosAlcanzables transiciones estadosNuevos (nub (estadosNuevos ++ alcanzados))
  

estadoAlcanzableAux :: [([String], Char, [String])] -> [String] -> [[String]]
estadoAlcanzableAux transiciones estado = nub (map (\(s,_,f) -> if s == estado then f else []) transiciones)

-- conjuntoPotencia :: Estados -> ConjuntoPotenciaDeLosEstados
conjuntoPotencia :: [String] -> [[String]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (estado:resto) = let restoPotencia = conjuntoPotencia(resto) in
  nub (restoPotencia ++ (map (\x -> estado:x) restoPotencia))

-- transicionesPotencia :: Transiciones -> PotenciaDeEstados -> Alfabeto -> EstadosResultantes
transicionesPotencia :: [(String, Char, [String])] -> [[String]] -> [Char] -> [([String], Char, [String])]
transicionesPotencia _ _ [] = []
transicionesPotencia transiciones estados (a:alfabeto) = transicionesPotenciaAux transiciones estados (a:[]) ++ transicionesPotencia transiciones estados alfabeto

-- transicionesPotenciaAux :: Transiciones -> PotenciaDeEstados -> Simbolo del alfabeto -> TransicionesResultantes 
transicionesPotenciaAux :: [(String, Char, [String])] -> [[String]] -> [Char]  -> [([String], Char, [String])]
transicionesPotenciaAux transiciones estados (a:xs) =
  (map (\q -> (q, a, (nub (transicionAFN transiciones q (a:xs) )))) estados)

