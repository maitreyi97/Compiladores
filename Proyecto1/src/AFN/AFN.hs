module AFN.AFN where

import Data.List (nub, sort, intercalate)
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

-- En AFN/AFN.hs
-- ... (tus otras funciones como aceptaAFN, etc. van aquí arriba)

-- ==========================================================
-- VERSIÓN CORREGIDA DE afn_afd
-- ==========================================================
afn_afd :: AFN -> AFD
afn_afd (AFN estados afnAlfabeto q0 afnTransiciones afnFinales) =
  let
    q0_afd = nub $ sort [q0]

    -- Función principal que explora los estados del AFD recursivamente.
    -- `pendientes`: lista de estados del AFD (que son `[String]`) que necesitamos procesar.
    -- `descubiertos`: lista de todos los estados del AFD que ya hemos encontrado.
    -- `transicionesAcc`: acumulador para las transiciones del nuevo AFD.
    
    -- CORRECCIÓN: Se ajustó la firma de tipo para que coincida con la lógica.
    explorar :: [[String]] -> [[String]] -> [([String], Char, [String])] -> ([[String]], [([String], Char, [String])])
    explorar [] descubiertos transicionesAcc = (descubiertos, transicionesAcc)
    explorar (q_actual:pendientes) descubiertos transicionesAcc =
      let
        nuevasTransiciones = map (\simbolo ->
            let destino = nub $ sort $ concatMap (\sub_q -> auxiliarTransicion afnTransiciones sub_q simbolo) q_actual
            in (q_actual, simbolo, destino)
          ) afnAlfabeto

        nuevosDescubiertos = nub $ filter (\q -> not (null q) && not (q `elem` descubiertos)) (map (\(_,_,d) -> d) nuevasTransiciones)

      in explorar (pendientes ++ nuevosDescubiertos) (descubiertos ++ nuevosDescubiertos) (transicionesAcc ++ nuevasTransiciones)

    -- Inicia la exploración.
    (estadosAlcanzables, transicionesAFD_sin_formato) = explorar [q0_afd] [q0_afd] []

    finalesAFD = filter (\q_afd -> any (`elem` afnFinales) q_afd) estadosAlcanzables

    -- Formateamos los estados (conjuntos de strings) a un único string para cumplir con el tipo AFD.
    formatEstado = intercalate "" . sort
    estadosAFD_formateados = map formatEstado estadosAlcanzables
    inicialAFD_formateado = formatEstado q0_afd
    
    -- CORRECCIÓN: Se aplica el formato a la lista de transiciones correcta.
    transicionesAFD_formateadas = map (\(q1, s, q2) -> (formatEstado q1, s, formatEstado q2)) transicionesAFD_sin_formato
    
    finalesAFD_formateados = map formatEstado finalesAFD

  in AFD estadosAFD_formateados afnAlfabeto inicialAFD_formateado transicionesAFD_formateadas finalesAFD_formateados
-- | Función encargada de revisar que estados son alcanzables a partir de una lista de transiciones y un estado inicial.
-- Prueba recursivamente los estados que son alcanzables, una vez que hemos alcanzado un nuevo estado se añade a la lista de estados alcanzados y posteriormente si ya no quedan estados por verificar se termina la función regresando todos los estados alcanzados. 
-- estadosAlcanzables :: Transiciones -> EstadosActuales -> EstadosVistos -> EstadosAlcanzables
estadosAlcanzables :: [([String], Char, [String])] -> [[String]] -> [[String]] -> [[String]]
estadosAlcanzables _ [] alcanzados = alcanzados
estadosAlcanzables transiciones estadosActuales alcanzados =
  let estadosSiguientes = concat (nub (map (\q -> estadoAlcanzableAux transiciones q) estadosActuales))
      estadosNuevos = filter (\q -> (not (null q) && (not (q `elem` alcanzados)))) estadosSiguientes
  in estadosAlcanzables transiciones estadosNuevos (nub (estadosNuevos ++ alcanzados))
  
--- | Funcion auxiliar de estados alcanzables, regresa los estados alcanzados a partir de un estado en particular. 
estadoAlcanzableAux :: [([String], Char, [String])] -> [String] -> [[String]]
estadoAlcanzableAux transiciones estado = nub (map (\(s,_,f) -> if s == estado then f else []) transiciones)

-- | Regresa el conjunto potencia de los estados
-- conjuntoPotencia :: Estados -> ConjuntoPotenciaDeLosEstados
conjuntoPotencia :: [String] -> [[String]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (estado:resto) = let restoPotencia = conjuntoPotencia(resto) in
  nub (restoPotencia ++ (map (\x -> estado:x) restoPotencia))

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

