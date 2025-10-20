module MDD.MDD where
import Regex.Regex
import Regex.SpecReader
import AFD.AFD
import AFNE.AFNE
import AFN.AFN

type Estado = String
type Simbolo = Char

data MDD = MDD {
    estados :: [Estado],
    alfabeto :: [Simbolo],
    transiciones :: [Estado, Simbolo, Estado],  
    estadoInicial :: Estado,
    finales :: [Estado],
    mu :: [(Estado, Categoria)],             
    errorLexico :: Categoria                
} deriving (Show)


--construirMDD :: [Categorias] -> MDD
--construirMDD categorias =


generarAFDMin :: RegEx -> AFD
generarAFDMin regex =
  let afne = regexToAFNE regex
      afn = afne_to_afn afne
      afd = afn_afd afn
  in minimizarAFD afd

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
    transicionesOriginales = (concatMap (\(AFD _ _ _ transiciones _) -> (map (\(t1, s, t2) -> (t1, SimboloE s, [e2])) transiciones)) afds)
    transicionesEpsilon = map (\(AFD _ _ q _ _) -> (nuevoInicial, Eps, [q])) afds
    todosFinales = nub (concatMap (\(AFD _ _ _ _ fs) -> fs) afds)
  in (AFNE todosLosEstados todosLosAlfabetos nuevoInicial (transicionesOriginales ++ transicionesEpsilon) todosFinales)     
