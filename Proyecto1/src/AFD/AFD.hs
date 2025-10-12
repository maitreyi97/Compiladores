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

-- minimizarAFD :: AFD -> AFD Minimizado
minimizarAFD :: AFD -> AFD
minimizarAFD (AFD estados alfabeto estadoInicial transiciones estadosFinales) =
  let pares = (generarTabla transiciones (revisarCeldas (generarPares(estados)) estadosFinales) alfabeto)
      nuevosEstados = colapsarEstados pares
      nuevasTransiciones = colapsarTransiciones nuevosEstados pares transiciones alfabeto
      nuevoEstadoInicial = encontrarRepresentante pares estadoInicial
      nuevoEstadoFinal = nub [encontrarRepresentante pares e | e <- estadosFinales] in
    (AFD nuevosEstados alfabeto nuevoEstadoInicial nuevasTransiciones nuevoEstadoFinal)

-- generarTabla :: transiciones -> pares -> alfabeto -> nuevo pares sin cambios
generarTabla :: [(String, Char, String)] -> [(String, [String])] -> [Char] -> [(String, [String])]
generarTabla transiciones pares alfabeto =
  let nuevoPar = (revisarTransiciones transiciones pares alfabeto) in
    if (sinCambios pares nuevoPar) then pares else (generarTabla transiciones nuevoPar alfabeto)

-- colapsarTransiciones :: Estados nuevos -> pares -> transiciones -> alfabeto -> transicionesNuevas
colapsarTransiciones :: [String] -> [(String, [String])] -> [(String, Char, String)] -> [Char] -> [(String, Char, String)]
colapsarTransiciones nuevosEstados pares transicionesOriginales alfabeto =
  let nuevaTransicion = [(nuevoEstado, simbolo, nuevoDestino)
                        | nuevoEstado <- nuevosEstados,
                          simbolo <- alfabeto,
                          let maybeDestinoOriginal = buscarTransicion transicionesOriginales nuevoEstado simbolo,
                          isJust maybeDestinoOriginal,
                          let destinoOriginal = quitarMaybe maybeDestinoOriginal,
                          let nuevoDestino = encontrarRepresentante destinoOriginal pares
                        ] in (nub nuevaTransicion)


encontrarRepresentante :: [(String, [String])] -> String -> String
encontrarRepresentante [] estado = estado
encontrarRepresentante ((p, listaQs):resto) estado
  | estado == p || estado `elem` listaQs = p
  | otherwise = encontrarRepresentante resto estado
    
-- colapsarEstados :: Pares de Estados iguales -> Estados
colapsarEstados :: [(String, [String])] -> [String]
colapsarEstados [] = []
colapsarEstados ((p, []):resto) = p:(colapsarEstados resto)
colapsarEstados ((p, listaQs):resto) = let estadosObtenidos = (map (\x -> (removerEstado resto x)) listaQs) in
  colapsarEstados ((p, estadosObtenidos):resto)

-- removerEstado :: pares con el estado a remover -> estado a remover -> estados iguales en el par removido
removerEstado :: [(String, [String])] -> String -> [String]
removerEstado [] estado = []
removerEstado ((p, listaQs):resto) remover = if p == remover then listaQs else removerEstado resto remover

-- generarPares :: Estados -> Pares de estados
generarPares :: [String] -> [(String, [String])]
generarPares [] = []
generarPares (estado:resto) = (estado,resto):(generarPares resto)

-- revisarCeldas :: Pares de estados -> Estados Finales -> Pares de estados
revisarCeldas :: [(String, [String])] -> [String] -> [(String, [String])]
revisarCeldas [] finales = []
revisarCeldas ((estado,pares):resto) finales  = (estado, (filter (\ s -> ( ((estado `elem` finales) && (not(s `elem` finales)))  || ((not (estado `elem` finales)) && (s `elem` finales)))) pares)):(revisarCeldas resto finales)   

-- revisarTransiciones :: Transiciones -> Pares de Estados -> Alfabeto -> ParesDeEstados
revisarTransiciones :: [(String, Char, String)] -> [(String, [String])] -> [Char] -> [(String, [String])]
revisarTransiciones transiciones pares alfabeto = (map (\(x,y) -> (x, (revisarMarcadoConTransicion transiciones pares x y alfabeto))) pares) 

-- EstaMarcado :: Estado 1 -> Estado 2 -> Pares De Estados -> True si esta en pares de estados, false si no está
estaMarcado :: String -> String -> [(String,[String])] -> Bool
estaMarcado p q [] = False
estaMarcado p q ((p1, listaQs):xs)
  | p == p1 = not (q `elem` listaQs) || (estaMarcado p q xs)
  | q == p1 = not (p `elem` listaQs) || (estaMarcado p q xs)
  | otherwise = estaMarcado p q xs

-- revisarMarcadocontransicion :: Transiciones -> Pares -> Estado -> Lista de Estados Emparejados -> Alfabeto -> Nueva Lista de Estados Emparejados  
revisarMarcadoConTransicion :: [(String, Char, String)] -> [(String, [String])] -> String -> [String] -> [Char] -> [String]
revisarMarcadoConTransicion transiciones pares estado listaQs [] = listaQs
revisarMarcadoConTransicion transiciones pares estado listaQs (a:alfabeto) =
  let estadosTransicionados = (map (\x -> quitarMaybe (buscarTransicion transiciones x a)) listaQs) in
    (filter (\q -> (estaMarcado (quitarMaybe (buscarTransicion transiciones estado a)) (quitarMaybe (buscarTransicion transiciones q a)) pares)) estadosTransicionados)

-- quitarmaybe Maybe Estado -> Estado o Estado de Rechazo
quitarMaybe :: Maybe String -> String
quitarMaybe (Just s) = s
quitarMaybe Nothing = "rechaazado"

-- sinCambios :: Pares antes de las transiciones -> Pares después de las transiciones -> Bool sobre si ya no hay modificaciones
sinCambios :: [(String, [String])] -> [(String, [String])] -> Bool
sinCambios [] [] = True
sinCambios ((p, listaQ):xs) ((p2, listaQ2):ys) = if p == p2 && listaQ == listaQ2 then
        (sinCambios xs ys) else False
