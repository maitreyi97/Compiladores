module MDD.MDD where

import Regex.Regex
import Regex.SpecReader
import AFD.AFD
import AFNE.AFNE
import AFN.AFN
import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, catMaybes)

type EstadoMDD = String
type Simbolo = Char
type CategoriaNombre = String 

data MDD = MDD {
    estadosMDD :: [EstadoMDD], -- estados del MDD             
    alfabetoMDD :: [Simbolo], -- alfabeto del MDD                
    transicionesMDD :: [(EstadoMDD, Simbolo, EstadoMDD)],  -- transiciones del MDD
    estadoInicialMDD :: EstadoMDD,  --nuevo estado inicial que creamos para contruir el MDD              
    estadosFinalesMDD :: [EstadoMDD],  -- nuevos estados finales del MDD           
    funcionMu :: Map EstadoMDD CategoriaNombre,  -- función mu que mapea estados finales a nombres de categorías      
    simboloError :: String -- símbolo de error para manejar cadenas no reconocidas o indicar un error léxico                       
} deriving (Show)

-- Función que construye el MDD a partir de una lista de categorías (nombre y expresión regular)
-- Vamos a generar un AFD minimizado para cada categoría reenombrado para evitar conflictos
-- 
construirMDD :: [Categoria] -> MDD
construirMDD categorias =
  let
    afdsMinimizados = map (\(nombre, regex) -> (nombre, (renombrarAFD (generarAFDMin regex) (nombre ++ "_")))) categorias
    mapFinalesACat = Map.fromList (concatMap (\(nombre, (AFD _ _ _ _ finales)) -> (map (\q -> (q, nombre)) finales)) afdsMinimizados)
    unificado =  afn_afd (afne_to_afn (unificarAFDs afdsMinimizados "mdd_inicial"))
    (estadosFinales, mu) = construirFuncionMu (getFinales unificado) mapFinalesACat (map fst categorias)
  in  aux unificado mu estadosFinales
  where
    aux :: AFD -> Map String CategoriaNombre -> [String] -> MDD
    aux (AFD estados alfabeto estadoInicial transiciones estadoFinal) fmu nuevosFinales =
      MDD
      {
        estadosMDD = estados,
        alfabetoMDD = alfabeto,
        transicionesMDD = transiciones,
        estadoInicialMDD = estadoInicial, 
        estadosFinalesMDD = nuevosFinales,
        funcionMu = fmu,
        simboloError = "ErrorLexico"
      }

-- Función que obtiene los estados finales de un AFD
-- Regresa una lista con los estados finales
getFinales :: AFD -> [String]
getFinales (AFD _ _ _ _ q) = q

-- Función que genera un AFD minimizado a partir de una expresión regular
generarAFDMin :: RegEx -> AFD
generarAFDMin regex = minimizarAFD (afn_afd (afne_to_afn (regexToAFNE regex)))

-- Función que renombra los estados de un AFD con un prefijo
renombrarAFD :: AFD -> String -> AFD
renombrarAFD (AFD estados alfabeto estadoInicial transiciones estadoFinal ) prefijo =
  let ren e = prefijo ++ e
  in (AFD (map ren estados) alfabeto (ren estadoInicial) (map (\(e1, s, e2) -> (ren e1, s, ren e2)) transiciones) (map ren estadoFinal))

-- Función que unifica varios AFDs en un solo AFNE
-- Agrega un nuevo estado inicial con transiciones epsilon a los estados iniciales de cada AFD
unificarAFDs :: [(String, AFD)] -> String -> AFNE
unificarAFDs categorias nuevoInicial =
  let
    afds = map snd categorias
    todosLosEstados = nub (nuevoInicial : (concatMap (\(AFD qs _ _ _ _) -> qs ) afds))
    todosLosAlfabetos = nub (concatMap (\(AFD _ alf _ _ _) -> (map (\x -> SimboloE x) alf)) afds)
    transicionesOriginales = (concatMap (\(AFD _ _ _ transiciones _) -> (map (\(t1, s, t2) -> (t1, SimboloE s, [t2])) transiciones)) afds)
    transicionesEpsilon = map (\(AFD _ _ q _ _) -> (nuevoInicial, Eps, [q])) afds
    todosFinales = nub (concatMap (\(AFD _ _ _ _ fs) -> fs) afds)
  in (AFNE todosLosEstados todosLosAlfabetos nuevoInicial (transicionesOriginales ++ transicionesEpsilon) todosFinales)     
-- 
-- Función que construye la función mu del MDD
-- 
construirFuncionMu :: [String] -> Map String CategoriaNombre -> [CategoriaNombre] -> ([String], Map String CategoriaNombre)
construirFuncionMu estadosFinalesMDD mapaFinalesCategoria ordenCategorias =
  let
    mapeo = map (\ef ->
                   let
                     categorias = catMaybes (map (\c ->
                                                    if any (\(estadoFinal, _) -> estadoFinal `isInfixOf` ef) (Map.toList (Map.filter (==c) mapaFinalesCategoria))
                                                    then Just c
                                                    else Nothing
                                                 ) ordenCategorias)
                     categoriaFinal = head categorias
                   in (ef, categoriaFinal)
                ) estadosFinalesMDD
    nuevosFinales = map fst mapeo
    funcionMuMap = Map.fromList mapeo
  in (nuevosFinales, funcionMuMap)
      
-- Funciones de transición del MDD
-- Función de transición del MDD que busca la transición correspondiente al estado y símbolo dados
transicionMDD :: MDD -> EstadoMDD -> Simbolo -> Maybe EstadoMDD
transicionMDD mdd estado simbolo = buscarTransicion (transicionesMDD mdd)
  where
    buscarTransicion [] = Nothing
    buscarTransicion ((e1, s, e2):rest)
        | e1 == estado && s == simbolo = Just e2
        | otherwise = buscarTransicion rest

-- Función de transición extendida del MDD que procesa una cadena completa desde un estado inicial
transicionExtendida mdd cadena = 
    transicionExtendidaAux mdd (estadoInicialMDD mdd) cadena
  where
    transicionExtendidaAux _ estado [] = Just estado
    transicionExtendidaAux mdd estado (x:xs) =
        case transicionMDD mdd estado x of
            Just next -> transicionExtendidaAux mdd next xs
            Nothing -> Nothing

-- Función auxiliar para obtener el nombre de una categoría a partir del par
nombreCategoria :: Categoria -> String
nombreCategoria (nombre, _) = nombre

-- Función que implementa la regla del prefijo más largo
prefijoMaximo :: MDD -> String -> Maybe (String, String, String)
prefijoMaximo mdd w =
    let
        prefijosDescendentes = reverse (tail (inits w))
        prefijo = find esPrefijoValido prefijosDescendentes
        esPrefijoValido x =
            case transicionExtendida mdd x of
                Just q ->
                    q `elem` estadosFinalesMDD mdd &&
                    case Map.lookup q (funcionMu mdd) of
                        Just _ -> True
                        Nothing -> False
                Nothing -> False 
    
    in case prefijo of
        Just x_max ->
            let
                sufijo = drop (length x_max) w
                Just q = transicionExtendida mdd x_max 
                Just catNombre = Map.lookup q (funcionMu mdd) 
            in Just (x_max, sufijo, catNombre)
        Nothing -> Nothing

-- Función auxiliar para el manejo de comentario multilínea o de carácteres múltiples
consumirComentario :: String -> (String, String) -> (String, String)
consumirComentario "" (conComentario, sinComentario) = (conComentario, sinComentario)
consumirComentario ('\n':resto) (conComentario, sinComentario) = (conComentario, sinComentario ++ resto) 
consumirComentario (a:as) (conComentario, sinComentario) = consumirComentario as (conComentario ++ [a], sinComentario)

-- Función que realiza la segmentación de la cadena en tokens
muEstrella :: MDD -> String -> [(String, String)]
muEstrella mdd [] = []
muEstrella mdd w =
    case prefijoMaximo mdd w of
        Just (lexema, resto,"Comentarios") ->
            let
              consumido = consumirComentario resto (lexema, "")
              newLex = fst consumido
              newRes = snd consumido
            in ("Comentarios", newLex) : muEstrella mdd newRes
        Just (lexema, resto, cat) ->
            (cat, lexema) : muEstrella mdd resto
        Nothing ->
            case w of
                (' ':xs) -> muEstrella mdd xs
                ('\n':xs) -> muEstrella mdd xs
                ('\t':xs) -> muEstrella mdd xs
                (x:xs) -> (simboloError mdd, [x]) : muEstrella mdd xs

-- Lista de categorías que deben ser ignoradas en el resultado final
ignorados :: [String]
ignorados = ["Comentarios", "Espacio"]

-- Función que realiza el análisis léxico de la cadena utilizando la MDD
tokens :: MDD -> String -> [(String, String)]
tokens mdd cadena = muEstrella mdd cadena
  
