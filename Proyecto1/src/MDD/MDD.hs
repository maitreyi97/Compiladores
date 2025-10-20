module MDD.MDD where

import Regex.Regex
import Regex.SpecReader
import AFD.AFD
import AFNE.AFNE
import AFN.AFN
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

type EstadoMDD = String
type Simbolo = Char
type CategoriaNombre = String 

data MDD = MDD {
    estadosMDD :: [EstadoMDD],                   
    alfabetoMDD :: [Simbolo],                       
    transicionesMDD :: [(EstadoMDD, Simbolo, EstadoMDD)],  
    estadoInicialMDD :: EstadoMDD,                
    estadosFinalesMDD :: [EstadoMDD],             
    funcionMu :: [(EstadoMDD, CategoriaNombre)],        
    simboloError :: String                        
} deriving (Show)


construirMDD :: [Categorias] -> MDD
construirMDD categorias =
  where
    aux :: AFD -> [(EstadoMDD, CategoriaNombre)] -> [String] -> MDD
    aux (AFD estados, alfabeto estadoInicial transiciones estadoFinal) fmu nuevosFinales =
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
  let
    afdsMinimizados = map (\(nombre, regex) -> (nombre, (renombrarAFD (generarAFDmin regex) (nombre ++ "_")))) categorias
    mapFinalesACat = Map.fromList (concatMap (\(nombre, (AFD _ _ _ _ finales)) -> (map (\q -> (q, nombre)) finales)) afdsMinimizados)
    unificado =  afn_afd (afne_to_afn (unificarAFDs afdsMinimizados "mdd_inicial"))
    (estadosFinales, mu) = construirFuncionMu (getFinales q) mapFinalesACat (map fst categorias)
  in  aux unificado mu estadosFinales


getFinales :: AFD -> [String]
getFinales (AFD _ _ _ _ q) = q

generarAFDMin :: RegEx -> AFD
generarAFDMin regex = minimizarAFD (afn_afd (afne_to_afn (regexToAFNE regex)))


renombrarAFD :: AFD -> String -> AFD
renombrarAFD (AFD estados alfabeto estadoInicial transiciones estadoFinal ) prefijo =
  let ren e = prefijo ++ e
  in (AFD (map ren estados) alfabeto (ren estadoInicial) (map (\(e1, s, e2) -> (ren e1, s, ren e2)) transiciones) (map ren estadoFinal))

unificarAFDs :: [(String, AFD)] -> String -> AFNE
unificarAFDs categorias nuevoInicial =
  let
    afds = map snd categorias
    todosLosEstados = nub (nuevoInicial : (concatMap (\(AFD qs _ _ _ _) -> qs ) afds))
    todosLosAlfabetos = nub (concatMap (\(AFD _, alf, _, _, _) -> (map (\x -> SimboloE x) alf)) afds)
    transicionesOriginales = (concatMap (\(AFD _ _ _ transiciones _) -> (map (\(t1, s, t2) -> (t1, SimboloE s, [t2])) transiciones)) afds)
    transicionesEpsilon = map (\(AFD _ _ q _ _) -> (nuevoInicial, Eps, [q])) afds
    todosFinales = nub (concatMap (\(AFD _ _ _ _ fs) -> fs) afds)
  in (AFNE todosLosEstados todosLosAlfabetos nuevoInicial (transicionesOriginales ++ transicionesEpsilon) todosFinales)     
-- 

construirFuncionMu :: [String] -> Map String CategoriaNombre -> [CategoriaNombre] -> ([String], Map String String)
construirFuncionMu estadosFinalesMDD mapaFinalesCategoria ordenCategorias =
  let
    mapeo = map (\ef ->
                   let
                     maybeCategoria = find isJust (map (\c ->
                                                          if any (\(estadoFinal, _) -> estadoFinal `elem` ef) (Map.toList (Map.filter (==cat) mapaFinalesCategoria))
                                                          then Just c
                                                          else Nothing
                                                       ) ordenCategorias)
                                                  in (ef, fromJust maybeCategoria)
                ) estadosFinalesMDD
            nuevosFinales = map fst mapeo
            funcionMuMap = Map.fromList mapeo
      in (nuevosFinales, funcionMuMap)
      
transicionMDD :: MDD -> EstadoMDD -> Simbolo -> Maybe EstadoMDD
transicionMDD mdd estado simbolo = buscarTransicion (transicionesMDD mdd)
  where
    buscarTransicion [] = Nothing
    buscarTransicion ((e1, s, e2):rest)
        | e1 == estado && s == simbolo = Just e2
        | otherwise = buscarTransicion rest

transicionExtendida :: MDD -> String -> Maybe EstadoMDD
transicionExtendida mdd cadena = 
    transicionExtendidaAux mdd (estadoInicialMDD mdd) cadena
  where
    transicionExtendidaAux _ estado [] = Just estado
    transicionExtendidaAux mdd estado (x:xs) =
        case transicionMDD mdd estado x of
            Just next -> transicionExtendidaAux mdd next xs
            Nothing -> Nothing

---Función auxiliar
nombreCategoria :: CategoriaNombre -> String
nombreCategoria (nombre, _) = nombre

prefijoMaximo :: MDD -> String -> Maybe (String, String, String)
prefijoMaximo mdd w =
    let
        prefijosDescendentes = reverse (tail (inits w))
        prefijo = find esPrefijoValido prefijosDescendentes
        esPrefijoValido x =
            case transicionExtendida mdd x of
                Just q ->
                    q `elem` estadosFinalesMDD mdd &&
                    case lookup q (funcionMu mdd) of
                        Just _ -> True
                        Nothing -> False
                Nothing -> False -- No llegó a un estado
    
    in case prefijo of
        Just x_max ->
            let
                sufijo = drop (length x_max) w
                Just q = transicionExtendida mdd x_max 
                Just catTupla = lookup q (funcionMu mdd) 
                catNombre = nombreCategoria catTupla
            in Just (x_max, sufijo, catNombre)
        Nothing -> Nothing

muEstrella :: MDD -> String -> [(String, String)]
muEstrella mdd [] = []
muEstrella mdd w =
    case prefijoMaximo mdd w of
        Just (lexema, resto, cat) ->
            (cat, lexema) : muEstrella mdd resto
        Nothing ->
            case w of
                (x:xs) -> (simboloError mdd, [x]) : muEstrella mdd xs

