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

minimizarAFD :: AFD -> AFD
minimizarAFD (AFD estados alfabeto estadoInicial transiciones estadosFinales) = (AFD estados alfabeto estadoInicial transiciones estadosFinales)

colapsarTransiciones = [(String, [String])] -> [(String, Char, String)] -> [(String, Char, String)]

-- colapsarEstados :: Pares de Estados iguales -> Estados
colapsarEstados = [(String, [String])] -> [String]

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
revisarMarcadoConTransicion transiciones pares estado listaQs [] = pares
revisarMarcadoConTransicion transiciones pares estado listaQs (a:alfabeto) =
  let estadosTransicionados = map (\x ->  buscarTransicion transiciones x a) pares in
    (filter (\q -> (estaMarcado (quitarMaybe (buscarTransicion transiciones estado a)) (quitarMaybe (buscarTransicion transiciones q a)) pares) estadosTransicionados)

-- quitarmaybe Maybe Estado -> Estado o Estado de Rechazo
quitarMaybe :: Maybe String -> String
quitarMaybe (Just s) = s
quitarMaybe Nothing = "rechaazado"

-- sinCambios :: Pares antes de las transiciones -> Pares después de las transiciones -> Bool sobre si ya no hay modificaciones
sinCambios :: [(String, [String])] -> [(String, [String])] -> Bool
sinCambios [] [] = True
sinCambios ((p, listaQ):xs) ((p2, listaQ2):ys) = if p == p2 && listaQ == listaQ2 then
        (sinCambios xs ys) else False
