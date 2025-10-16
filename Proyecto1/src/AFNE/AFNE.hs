module AFNE.AFNE where

import Data.Set (toList, fromList)
import Regex.Regex

data SimboloE = SimboloE Char | Eps deriving (Eq, Ord, Show) 
type Estado = String 
type Estados = [Estado]
type AlfabetoE = [SimboloE]
type DeltaE = [(Estado, SimboloE, [Estado])]
type Inicial = Estado
type Finales = [Estado]

data AFNEp = AFNEp 
    { estados :: Estados,
     alfabetoE :: AlfabetoE,
     deltaE :: DeltaE,
     inicial :: Inicial,
     finales :: Finales
    } deriving (Show)

regexToAFNE :: RegEx -> AFNEp
regexToAFNE Epsilon = AFNEp ["q0", "q1"] [Eps] [("q0", Eps, ["q1"])] "q0" ["q1"]
regexToAFNE (Literal c) = AFNEp ["q0", "q1"] [SimboloE c] [("q0", SimboloE c, ["q1"])] "q0" ["q1"]
regexToAFNE (Concat l r) = AFNEp estados alfabeto delta inicial finales
  where AFNEp estadosL alfabetoL deltaL inicialL finalesL = regexToAFNE l
        AFNEp estadosR alfabetoR deltaR inicialR finalesR = regexToAFNE r
        estados = toList $ fromList (estadosL ++ estadosR)
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR)
        delta = deltaL ++ deltaR ++ [(finalL, Eps, [inicialR]) | finalL <- finalesL] 
        inicial = inicialL 
        finales = finalesR
regexToAFNE (Or l r) = AFNEp estados alfabeto delta inicial finales
  where AFNEp estadosL alfabetoL deltaL inicialL finalesL = regexToAFNE l
        AFNEp estadosR alfabetoR deltaR inicialR finalesR = regexToAFNE r
        estados = toList $ fromList ("q_0" : estadosL ++ estadosR) 
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR) 
        delta = deltaL ++ deltaR ++ [("q_0", Eps, [inicialL]), ("q_0", Eps, [inicialR])]
        inicial = "q_0"
        finales = finalesL ++ finalesR
regexToAFNE (Star e) = AFNEp estados alfabeto delta inicial finales
  where AFNEp estadosE alfabetoE deltaE inicialE finalesE = regexToAFNE e
        estados = estadosE
        alfabeto = alfabetoE 
        delta = deltaE ++ [ (finalE, Eps, [inicialE]) | finalE <- finalesE ] 
        inicial = inicialE
        finales = inicialE : finalesE
        


