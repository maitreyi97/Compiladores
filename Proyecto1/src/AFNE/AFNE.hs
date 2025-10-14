module AFNE.AFNE where

import Data.Set (toList, fromList)
import Regex.Regex as Regex

data SimboloE = SimboloE Char | Eps deriving (Eq, Ord, Show) 
type Estado = String 
type Estados = [Estado]
type AlfabetoE = [SimboloE]
type TransE = [(Estado, SimboloE, [Estado])]
type Inicial = Estado
type Finales = [Estado]

data AFNEp = AFNEp 
    { estados :: Estados,
     alfabetoE :: AlfabetoE,
     transE :: TransE,
     inicial :: Inicial,
     finales :: Finales
    } deriving (Show)

regex_to_afne :: RegEx -> AFNEp
regex_to_afne Epsilon = AFNEp ["q0", "q1"] [Eps] [("q0", Eps, ["q1"])] "q0" ["q1"]
regex_to_afne (Term c) = AFNEp ["q0", "q1"] [simbolo c] [("q0", simbolo c, ["q1"])] "q0" ["q1"]
regex_to_afne (And l r) = AFNEp estados alfabeto transiciones inicial finales
  where AFNEp estadosL alfabetoL transicionesL inicialL finalesL = regex_to_afne l
        AFNEp estadosR alfabetoR transicionesR inicialR finalesR = regex_to_afne r
        estados = toList $ fromList (estadosL ++ estadosR)
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR)
        transiciones = transicionesL ++ transicionesR ++ 
        [ (finalL, Eps, [inicialR]) | finalL <- finalesL ] 
        inicial = inicialL 
        finales = finalesR
regex_to_afne (Or l r) = AFNEp estados alfabeto transiciones inicial finales
  where AFNEp estadosL alfabetoL transicionesL inicialL finalesL = regex_to_afne l
        AFNEp estadosR alfabetoR transicionesR inicialR finalesR = regex_to_afne r
        estados = toList $ fromList ("q_0" : estadosL ++ estadosR) 
        alfabeto = toList $ fromList (alfabetoL ++ alfabetoR) 
        transiciones = transicionesL ++ transicionesR ++ 
        [("q_0", Eps, [inicialL]), ("q_0", Eps, [inicialR])]
        inicial = "q_0"
        finales = finalesL ++ finalesR
regex_to_afne (Kleene e) = AFNEp estados alfabeto transiciones inicial finales
  where AFNEp estadosE alfabetoE transicionesE inicialE finalesE = regex_to_afne e
        estados = estadosE
        alfabeto = alfabetoE 
        transiciones = transicionesE ++ [ (finalE, Eps, [inicialE]) | finalE <- finalesE ] 
        inicial = inicialE
        finales = inicialE : finalesE
        


