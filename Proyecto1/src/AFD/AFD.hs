module AFD.AFD where

import Data.Maybe
import Data.List (nub)


data AFD = AFD
  [String] -- Estados
  [Char] -- Alfabeto
  String -- Estado Inicial
  [(String, Char, String)] -- Transiciones
  [String] -- Estados Finales
  deriving (Show)
-- | Función Acepta que dada una cadena regresa si la cadena es aceptada por el AFD, esto lo sabe si el estado al terminar las transiciones forma parte de los estados finales.
acepta :: AFD -> String -> Bool
acepta (AFD estados alfabeto estadoInicial transiciones estadosFinales) cadena =
  ((transicion transiciones estadoInicial cadena) `elem` estadosFinales)

-- | Función encargada de realizar las transiciones de una cadena hasta que esta ya no tenga más simbolos para descartar. Si la transición es posible entonces seguiremos con la recursión, si no lo mandamos a un estado de rechazo.
transicion :: [(String, Char, String)] -> String -> String -> String
transicion _ estado "" = estado
transicion transiciones estado (x:xs) = case (buscarTransicion transiciones estado x) of
  Just estadoNuevo -> transicion transiciones estadoNuevo xs
  Nothing -> "EstadoRechazo"
  
-- | Función encargada de buscar la transición entre todas las transiciones hasta encontrar la que corresponda al estado y al simbolo, si no la encontramos regresamos Nothing, si lo hacemos, devolvemos el estado alcanzado al aplicar la transición 
buscarTransicion :: [(String, Char, String)] -> String -> Char -> Maybe String 
buscarTransicion [] _ _ = Nothing
buscarTransicion ((estadoInicial, caracter, estadoFinal):xs) estado simbolo
  | estadoInicial == estado && caracter == simbolo = Just estadoFinal
  | otherwise = buscarTransicion xs estado simbolo

-- | Función encargada de minimizar el AFD
-- La función comienza generando los pares de estados a partir de ls lista de estados, para despues revisar cuales de estos pares pertenece alguno a un estado final.
-- Posteriormente genera el resto de la tabla. Después genera los nuevos Estados a partir de los representantes de cada conjunto de estados que son equivalentes.
-- Posteriormente a partir de estos nuevos estados y los pares encontrados en las tablas se encarga de colapsar las transiciones para con ello generar nuevasTransiciones.
-- Posteriormente buscamos el nuevo estado inicial a partir de encontrar el representante del estado inicial en la lista de pares
-- Realizamos el mismo proceso anterior pero ahora con los estados finales
-- Finalmente generamos el AFD. 
-- minimizarAFD :: AFD -> AFD Minimizado
minimizarAFD :: AFD -> AFD
minimizarAFD (AFD estados alfabeto estadoInicial transiciones estadosFinales) =
  let pares = (generarTabla transiciones (revisarCeldas (generarPares(estados)) estadosFinales) alfabeto)
      nuevosEstados = nub [encontrarRepresentante pares e | e <- estados]
      nuevasTransiciones = colapsarTransiciones nuevosEstados pares transiciones alfabeto
      nuevoEstadoInicial = encontrarRepresentante pares estadoInicial
      nuevoEstadoFinal = nub [encontrarRepresentante pares e | e <- estadosFinales] in
    (AFD nuevosEstados alfabeto nuevoEstadoInicial nuevasTransiciones nuevoEstadoFinal)

-- | Funcion encargada de generar la tabla correspondiente al algoritmo y de completarla hasta que ya no haya cambios en los estados marcados 
-- generarTabla :: transiciones -> pares -> alfabeto -> nuevo pares sin cambios
generarTabla :: [(String, Char, String)] -> [(String, [String])] -> [Char] -> [(String, [String])]
generarTabla transiciones pares alfabeto =
  let nuevoPar = (revisarTransiciones transiciones pares alfabeto) in
    if (sinCambios pares nuevoPar) then pares else (generarTabla transiciones nuevoPar alfabeto)

-- | Función encargada de colapsar las transiciones a partir de los estados nuevos y los pares hacia transiciones nuevas, las cuales formarán parte del AFD.
-- colapsarTransiciones :: Estados nuevos -> pares -> transiciones -> alfabeto -> transicionesNuevas
colapsarTransiciones :: [String] -> [(String, [String])] -> [(String, Char, String)] -> [Char] -> [(String, Char, String)]
colapsarTransiciones nuevosEstados pares transicionesOriginales alfabeto =
  let nuevaTransicion = [(nuevoEstado, simbolo, nuevoDestino)
                        | nuevoEstado <- nuevosEstados,
                          simbolo <- alfabeto,
                          let maybeDestinoOriginal = buscarTransicion transicionesOriginales nuevoEstado simbolo,
                          isJust maybeDestinoOriginal,
                          let destinoOriginal = quitarMaybe maybeDestinoOriginal,
                          let nuevoDestino = encontrarRepresentante pares destinoOriginal
                        ] in (nub nuevaTransicion)

-- | Funcion encargada de buscar el representante del conjunto de estados equivalentes
-- La función buscará la primera aparición del estado en cuestion en los pares, ya sea como el elemento representante o como el elemento equivalente. 
encontrarRepresentante :: [(String, [String])] -> String -> String
encontrarRepresentante [] estado = estado
encontrarRepresentante ((p, listaQs):resto) estado
  | estado == p || estado `elem` listaQs = p
  | otherwise = encontrarRepresentante resto estado

-- | Función encargada de generar los pares de estados que conforman la tabla.
-- generarPares :: Estados -> Pares de estados
generarPares :: [String] -> [(String, [String])]
generarPares [] = []
generarPares (estado:resto) = (estado,resto):(generarPares resto)

-- | La función como el algorimto lo indica marcará a aquellos elementos que uno de sus estados pertenezca a los estados finales y el otro elemento no pertenezca a los estados finales, dado que no tenemos como marcar los elementos y al final estos serán descartados, se procedió a descartarlos directamente.

-- revisarCeldas :: Pares de estados -> Estados Finales -> Pares de estados
revisarCeldas :: [(String, [String])] -> [String] -> [(String, [String])]
revisarCeldas [] finales = []
revisarCeldas ((estado,pares):resto) finales  = (estado, (filter (\ s -> ( ((estado `elem` finales) && ((s `elem` finales)))  || ((not (estado `elem` finales)) && ( not (s `elem` finales))))) pares)):(revisarCeldas resto finales)   

-- | Función encargada de a partir de las transiciones si alguno de estos estados te manda a un estado marcado, esto se realizará con un map en la función de pares para cada uno de los pares de nuestra lista de pares 
-- revisarTransiciones :: Transiciones -> Pares de Estados -> Alfabeto -> ParesDeEstados
revisarTransiciones :: [(String, Char, String)] -> [(String, [String])] -> [Char] -> [(String, [String])]
revisarTransiciones transiciones pares alfabeto = (map (\(x,y) -> (x, (revisarMarcadoConTransicion transiciones pares x y alfabeto))) pares) 

-- | Función encargada de revisar si un estado está marcado o no
-- EstaMarcado :: Estado 1 -> Estado 2 -> Pares De Estados -> True si esta en pares de estados, false si no está
estaMarcado :: String -> String -> [(String,[String])] -> Bool
estaMarcado p q _ | p == q = False
estaMarcado _ _ [] = True
estaMarcado p q ((p1, listaQs):xs)
  | p == p1 && (q `elem` listaQs) = False
  | q == p1 && (p `elem` listaQs) = False
  | otherwise = estaMarcado p q xs

-- | Función encargada de revisar que estados podrían estar marcados al realizar una transicion, esto se realizará filtrando los estados que alguna transición se encuentre marcada, entonces se marcará y de la misma forma se quitará de la lista. Por ello es que se usa el filter.  
-- revisarMarcadocontransicion :: Transiciones -> Pares -> Estado -> Lista de Estados Emparejados -> Alfabeto -> Nueva Lista de Estados Emparejados  
revisarMarcadoConTransicion :: [(String, Char, String)] -> [(String, [String])] -> String -> [String] -> [Char] -> [String]
revisarMarcadoConTransicion transiciones pares estado listaQs alfabeto = filter (\q -> (all (\a -> (not (estaMarcado (quitarMaybe (buscarTransicion transiciones estado a)) (quitarMaybe (buscarTransicion transiciones q a)) pares))) alfabeto)) listaQs

-- quitarmaybe Maybe Estado -> Estado o Estado de Rechazo
quitarMaybe :: Maybe String -> String
quitarMaybe (Just s) = s
quitarMaybe Nothing = "rechaazado"

-- | Función que se encarga de ver si ha habido un cambio en los pares de estados.
-- sinCambios :: Pares antes de las transiciones -> Pares después de las transiciones -> Bool sobre si ya no hay modificaciones
sinCambios :: [(String, [String])] -> [(String, [String])] -> Bool
sinCambios [] [] = True
sinCambios ((p, listaQ):xs) ((p2, listaQ2):ys) = if p == p2 && listaQ == listaQ2 then
        (sinCambios xs ys) else False

