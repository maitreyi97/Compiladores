module AFNE.AFNE where

import Data.Set (toList, fromList)
import Regex.Regex
import Regex.SpecReader

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
regexToAFNE re = fst (regexToAFNEaux re 0)

regexToAFNEaux :: RegEx -> Int -> (AFNE, Int)
regexToAFNEaux Epsilon n = (AFNE [q0, q1] [Eps] [(q0, Eps, [q1])] q0 [q1], n+2)
    where q0 = "q" ++ show n
          q1 = "q" ++ show (n+1)
regexToAFNEaux (Literal c) n = (AFNE [q0, q1] [SimboloE c] [(q0, SimboloE c, [q1])] q0 [q1], n+2)
    where q0 = "q" ++ show n
          q1 = "q" ++ show (n+1)
regexToAFNEaux (Concat l r) n = (AFNE estados alfabeto delta inicial finales, nR)
  where (afneL, nL) = regexToAFNEaux l n
        (afneR, nR) = regexToAFNEaux r nL
        AFNE estadosL alfabetoL deltaL inicialL finalesL = afneL
        AFNE estadosR alfabetoR deltaR inicialR finalesR = afneR
        estados = (estadosL ++ estadosR)
        alfabeto =  (alfabetoL ++ alfabetoR)
        delta = deltaL ++ deltaR ++ [(finalL, Eps, [inicialR]) | finalL <- finalesL]
        inicial = inicialL
        finales = finalesR
regexToAFNEaux (Or l r) n = (AFNE estados alfabeto delta inicial finales, nR)
  where (afneL, nL) = regexToAFNEaux l n
        (afneR, nR) = regexToAFNEaux r nL
        AFNE estadosL alfabetoL deltaL inicialL finalesL = afneL
        AFNE estadosR alfabetoR deltaR inicialR finalesR = afneR
        q0 = "q" ++ show n
        estados = (q0 : estadosL ++ estadosR)
        alfabeto = (alfabetoL ++ alfabetoR)
        delta = deltaL ++ deltaR ++ [(q0, Eps, [inicialL]), (q0, Eps, [inicialR])]
        inicial = q0
        finales = finalesL ++ finalesR
regexToAFNEaux (Star e) n = (AFNE estados alfabeto delta inicial finales, nE)
  where (AFNE estadosE alfabetoE deltaE inicialE finalesE, nE) = regexToAFNEaux e n 
        estados = estadosE
        alfabeto = alfabetoE
        delta = deltaE ++ [ (finalE, Eps, [inicialE]) | finalE <- finalesE ]
        inicial = inicialE
        finales = inicialE : finalesE
regexToAFNEaux (Plus e) n = (AFNE estados alfabeto delta inicial finales, nE)
  where (AFNE estadosE alfabetoE deltaE inicialE finalesE, nE) = regexToAFNEaux e n
        estados = estadosE
        alfabeto = alfabetoE
        delta = deltaE ++ [ (finalE, Eps, [inicialE]) | finalE <- finalesE ]
        inicial = inicialE
        finales = finalesE

categoriaToAFNE :: Categoria -> (String, AFNE)
categoriaToAFNE (cat, regex) = (cat, regexToAFNE regex)

categoriasToAFNEs :: [Categoria] -> [(String, AFNE)]
categoriasToAFNEs = foldr ((:) . categoriaToAFNE) []

transiciona :: AFNE -> Estado -> SimboloE -> [Estado]
transiciona (AFNE _ _ deltaE _ _) estado simbolo =
    concat [estadosFinales | (estadoInicial, simboloTransicion, estadosFinales) <- deltaE,
                             estadoInicial == estado,
                             simboloTransicion == simbolo]

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

