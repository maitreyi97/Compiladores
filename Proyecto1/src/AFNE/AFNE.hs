module AFNE.AFNE where

import Data.Set (toList, fromList)
import Regex.Regex
import Regex.SpecReader
import AFN.AFN
import Data.List (nub)

data SimboloE = SimboloE Char | Eps deriving (Eq, Ord, Show)
type Estado = String
type Estados = [Estado]
type AlfabetoE = [SimboloE]
type DeltaE = [(Estado, SimboloE, [Estado])]
type Inicial = Estado
type Finales = [Estado]

data AFNE = AFNE
    { estados :: Estados,
     alfabetoE :: AlfabetoE,
     deltaE :: DeltaE,
     inicial :: Inicial,
     finales :: Finales
    } deriving (Show)

regexToAFNE :: RegEx -> AFNE
regexToAFNE Epsilon = AFNE ["q0", "q1"] [Eps] [("q0", Eps, ["q1"])] "q0" ["q1"]
regexToAFNE (Literal c) = AFNE ["q0", "q1"] [SimboloE c] [("q0", SimboloE c, ["q1"])] "q0" ["q1"]
regexToAFNE (Concat l r) = AFNE estados alfabeto delta inicial finales
  where AFNE estadosL alfabetoL deltaL inicialL finalesL = regexToAFNE l
        AFNE estadosR alfabetoR deltaR inicialR finalesR = regexToAFNE r
        estados = toList $ fromList (estadosL ++ estadosR)
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR)
        delta = deltaL ++ deltaR ++ [(finalL, Eps, [inicialR]) | finalL <- finalesL]
        inicial = inicialL
        finales = finalesR
regexToAFNE (Or l r) = AFNE estados alfabeto delta inicial finales
  where AFNE estadosL alfabetoL deltaL inicialL finalesL = regexToAFNE l
        AFNE estadosR alfabetoR deltaR inicialR finalesR = regexToAFNE r
        estados = toList $ fromList ("q_0" : estadosL ++ estadosR)
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR)
        delta = deltaL ++ deltaR ++ [("q_0", Eps, [inicialL]), ("q_0", Eps, [inicialR])]
        inicial = "q_0"
        finales = finalesL ++ finalesR
regexToAFNE (Star e) = AFNE estados alfabeto delta inicial finales
  where AFNE estadosE alfabetoE deltaE inicialE finalesE = regexToAFNE e
        estados = estadosE
        alfabeto = alfabetoE
        delta = deltaE ++ [ (finalE, Eps, [inicialE]) | finalE <- finalesE ]
        inicial = inicialE
        finales = inicialE : finalesE

categoriaToAFNE :: Categoria -> (String, AFNE)
categoriaToAFNE (cat, regex) = (cat, regexToAFNE regex)

categoriasToAFNEs :: [Categoria] -> [(String, AFNE)]
categoriasToAFNEs = foldr ((:) . categoriaToAFNE) []

transiciona :: AFNE -> Estado -> SimboloE -> [Estado]
transiciona (AFNE _ _ deltaE _ _) estado simbolo =
    concat [estadosFinales | (estadoInicial, simboloTransicion, estadosFinales) <- deltaE,
                             estadoInicial == estado,
                             simboloTransicion == simbolo]

-- AFNE to AFN

afne_to_afn :: AFNE -> AFN
afne_to_afn afne =
  let cerradurasEpsilon = epsilonClosureAFNE afne
      transicionesEpsilon = transicionesClosure afne cerradurasEpsilon
      transicionesEpsilonFormateadas = [(p,c,x) | (p, SimboloE c, x) <- transicionesEpsilon]
      alfabetoFormateado = [c | SimboloE c <- (alfabetoE afne)]
      estadosFinales = [q | (q, qs) <- cerradurasEpsilon, any (`elem` finales afne) qs]
  in AFN (estados afne) alfabetoFormateado (inicial afne) transicionesEpsilonFormateadas estadosFinales

eClosure :: AFNE -> Estados -> Estados
eClosure afne estadosIniciales = closureAux estadosIniciales
  where
    closureAux :: Estados -> Estados
    closureAux actuales =
      let
        alcanzables = concatMap (\q -> transiciona afne q Eps) actuales
        proximo = nub (actuales ++ alcanzables)
      in
        if length proximo == length actuales then actuales else closureAux proximo


epsilonClosureAFNE :: AFNE -> [(Estado, [Estado])]
epsilonClosureAFNE afne = map (\q -> (q, eClosure afne [q])) (estados afne)

transicionesClosure :: AFNE -> [(Estado, [Estado])] -> DeltaE
transicionesClosure afne cerradurasEpsilon =
  filter (\(estado, simbolo, estadosL) -> if simbolo == Eps || estadosL == [] then False  else True) (concat (map (\(q,qs) ->  (map (\a -> (q,a,(nub (concat ( map (\x -> getEpsilonClosure cerradurasEpsilon x) (nub (concat (map (\p -> (transiciona afne p a)) qs))) )) ))) (alfabetoE afne) ) ) cerradurasEpsilon))

                                             
getEpsilonClosure :: [(Estado, Estados)] -> Estado -> Estados
getEpsilonClosure [] _ = []
getEpsilonClosure ((q, qs):xs) s = if s == q then qs else getEpsilonClosure xs s 

afnePrueba :: AFNE
afnePrueba = AFNE
  { estados = ["q0", "q1", "q2"],
    alfabetoE = [SimboloE 'a', SimboloE 'b', SimboloE 'c', Eps],
    deltaE = [("q0", SimboloE 'a', ["q0"]),
            ("q0", Eps, ["q1"]),
            ("q1", SimboloE 'b', ["q1"]),
            ("q1", Eps, ["q2"]),
            ("q2", SimboloE 'c', ["q2"])
           ],
    inicial = "q0",
    finales = ["q0", "q1", "q2"]
  }

ejemploAFNE :: AFNE
ejemploAFNE = AFNE
    { estados = ["q0", "q1", "q2"],
      alfabetoE = [SimboloE 'a', SimboloE 'b', Eps],
      deltaE = [ ("q0", SimboloE 'a', ["q0", "q1"]),
                 ("q0", Eps, ["q2"]),
                 ("q1", SimboloE 'b', ["q2"]),
                 ("q2", SimboloE 'a', ["q2"])
               ],
      inicial = "q0",
      finales = ["q2"]
    }

