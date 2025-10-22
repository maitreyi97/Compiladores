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

-- | Función encargada de revisar si una cadena es aceptada o no por nuestro AFN
-- Realiza las transiciones desde el estado inicial usando la cadena hasta que ya no quede cadena. Una vez terminado se encarga de revisar si alguno de los estados alcanzados son parte del estado final, si lo son regresa true, si no lo son regresa false.
aceptaAFN :: AFN -> String -> Bool
aceptaAFN (AFN estados alfabeto estadoInicial transiciones estadosFinales) cadena =
  or (map (`elem` (transicionAFN transiciones [estadoInicial] cadena)) estadosFinales)

-- | Función encargada de realizar las transiciones de un AFN correspondientes a una cadena proporcionada hasta que la cadena sea vacia, una vez que la cadena es vacia regresamos los estados alcanzados
-- Usa la recursión para consumir caracter por caracter y realizar transiciones de tal manera que cada caracter que consumimos realizamos su correspondiente transicion para cada uno de los estados ingresados, concatenamos el resultado para que vuelva a ser una lista de estados y continuamos.
transicionAFN :: [(String, Char, [String])] -> [String] -> String -> [String]
transicionAFN _ estadoActual "" = estadoActual
transicionAFN transiciones estados (x:xs) =
  transicionAFN transiciones (concat (map (\estadoActual -> auxiliarTransicion transiciones estadoActual x) estados)) xs

-- | Función encargada de realizar la transición dado un estado y un caracter revisa cual transición le corresponde y regresa la aplicación de esa transición.
auxiliarTransicion :: [(String, Char, [String])] -> String -> Char -> [String]
auxiliarTransicion [] _ _ = []
auxiliarTransicion ((estadoInicial, caracter, estadoFinal):cs) estadoActual simbolo
  | estadoInicial == estadoActual && simbolo == caracter = estadoFinal ++ auxiliarTransicion cs estadoActual simbolo
  | otherwise = auxiliarTransicion cs estadoActual simbolo

-- | Función encargada de transformar un AFN en AFD
-- Primero crea el conjuntoPotencia de los estados
-- Posteriormente crea la tabla de Transiciones sobre la cual se realiza el algoritmo para tranformar un AFN a unb AFN
-- Usando el resultado de la función anterior, se revisa que estados son factibles en nuestro nuevo automata, para no tener estados a los que no se pueda acceder
-- Posteriormente se revisa a partir de estos estados que estados son finales en el nuevo automata.
-- Se limpian las transiciones para que podramos generar nuevas transiciones a partir de los estados factibles y la tabla de transiciones.
-- Se formatean las cadenas de tal forma que formen un automata.

afn_afd :: AFN -> AFD
afn_afd (AFN estados alfabeto estadoInicial transiciones estadosFinales) =
  let potenciaEstados = estadosAlcanzables transiciones alfabeto [[estadoInicial]] [[estadoInicial]]
      tablaTransiciones = transicionesPotencia transiciones potenciaEstados alfabeto
      nuevosEstadosFinales = (filter (\x -> (or (map (\y -> (y `elem` x)) estadosFinales ))) potenciaEstados)
      estadosFactiblesFormateados = (map (\s -> (concat s)) potenciaEstados)
      transicionesFinalesFormateadas = (map (\(s0,c, s1) -> ((concat s0), c, (concat s1) )) tablaTransiciones)
      nuevosEstadosFinalesFormateados  = (map (\s -> (concat s)) nuevosEstadosFinales)
      in (AFD estadosFactiblesFormateados alfabeto estadoInicial transicionesFinalesFormateadas nuevosEstadosFinalesFormateados)

estadosAlcanzables :: [(String, Char, [String])] -> [Char]  -> [[String]] -> [[String]] -> [[String]]
estadosAlcanzables _ _ [] alcanzados = alcanzados
estadosAlcanzables transiciones alfabeto estadosActuales alcanzados =
  let estadosSiguientes = nub (concat (map (\q -> auxiliar transiciones alfabeto q) estadosActuales))
      estadosNuevos = filter (\q -> (not (null q) && (not (q `elem` alcanzados)))) estadosSiguientes
  in estadosAlcanzables transiciones alfabeto estadosNuevos (nub (estadosNuevos ++ alcanzados))
  where
    auxiliar :: [(String, Char, [String])] -> [Char] -> [String] -> [[String]]
    auxiliar transiciones alfabeto estados =
      map (\a -> nub (transicionAFN transiciones estados [a])) alfabeto


-- | Función encargada de revisar que estados son alcanzables a partir de una lista de transiciones y un estado inicial.
-- Prueba recursivamente los estados que son alcanzables, una vez que hemos alcanzado un nuevo estado se añade a la lista de estados alcanzados y posteriormente si ya no quedan estados por verificar se termina la función regresando todos los estados alcanzados. 
-- estadosAlcanzables :: Transiciones -> EstadosActuales -> EstadosVistos -> EstadosAlcanzables

-- Punto de control :: Eliminacion de una función 
  
--- | Funcion auxiliar de estados alcanzables, regresa los estados alcanzados a partir de un estado en particular. 


-- | Función encargada de regresar las nuevas transiciones que pueden obtenerse haciendo uso de una función auxiliar para poder sacar las transiciones potencia de todo el alfabeto 
-- transicionesPotencia :: Transiciones -> PotenciaDeEstados -> Alfabeto -> EstadosResultantes
transicionesPotencia :: [(String, Char, [String])] -> [[String]] -> [Char] -> [([String], Char, [String])]
transicionesPotencia _ _ [] = []
transicionesPotencia transiciones estados (a:alfabeto) = transicionesPotenciaAux transiciones estados (a:[]) ++ transicionesPotencia transiciones estados alfabeto

-- | Función encarga de dado un simbolo del alfabeto obtener todos los estados a los que se puede llegar para cada estado en la potencia de estados.
-- transicionesPotenciaAux :: Transiciones -> PotenciaDeEstados -> Simbolo del alfabeto -> TransicionesResultantes 
transicionesPotenciaAux :: [(String, Char, [String])] -> [[String]] -> [Char]  -> [([String], Char, [String])]
transicionesPotenciaAux transiciones estados (a:xs) =
  (map (\q -> (q, a, (nub (transicionAFN transiciones q (a:xs) )))) estados)

