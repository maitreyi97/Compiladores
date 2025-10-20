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
-- | Función encargada de transformar un AFNE a un AFN
-- La función consiste en la transformación de un AFNE a un AFN a través del proceso visto en la clase
-- Dicho proceso consiste en la formación de las cerraduras de Epsilon, la cual corresponde a los estados alcanzables, esta cerradura  es la que se utiliza para poder construir las nuevas transiciones que consideran los estados alcanzables a través de la epsilon transición
-- A través de transiciones epsilon, posteriormente estas transicionesEpsilon son filtradas para poder contener unicamente transiciones que no sean llevadas a cabo por algun epsilon
-- Seguido de esto formateamos el alfabeto para que este formado por unicamente caracteres (Tal como corresponde en AFN)
-- Posteriormente se generar los estados finales, verificando si alguno de los estados de la par de cerradura se encuentra en estado final.
-- Finalmente con los conjuntos generados es que construimos el AFN.

afne_to_afn :: AFNE -> AFN
afne_to_afn afne =
  let cerradurasEpsilon = epsilonClosureAFNE afne
      transicionesEpsilon = transicionesClosure afne cerradurasEpsilon
      transicionesEpsilonFormateadas = [(p,c,x) | (p, SimboloE c, x) <- transicionesEpsilon]
      alfabetoFormateado = [c | SimboloE c <- (alfabetoE afne)]
      estadosFinales = [q | (q, qs) <- cerradurasEpsilon, any (`elem` finales afne) qs]
  in AFN (estados afne) alfabetoFormateado (inicial afne) transicionesEpsilonFormateadas estadosFinales

-- | Función encargada de obtener la cerradura epsilon de un estado.
-- Dado un estado, recursivamente a través de las transiciones del afne, se encarga de buscar todos los estados alcanzables a traves de transiciones epsilon desde el estado actual, una vez que no se encuentre diferencia entre el los estados se regresan los estados obtenidos. en otro caso se continua en la recursión.

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

-- | Función encargada de obtener la cerradura epsilon de todos los estados del afne y ponerlo en un par ordenado junto con el estado del cual pertenece la cerradura

epsilonClosureAFNE :: AFNE -> [(Estado, [Estado])]
epsilonClosureAFNE afne = map (\q -> (q, eClosure afne [q])) (estados afne)

-- | Función encargada de realizar la nueva lista de transiciones que formarán parte del AFN, para ello utiliza una serie de funciones anidadas para poder realizar este proceso.
-- Funciones las cuales enumeraré desde la primera en realizarse hasta la ultima.
-- Por lo que la primer función en realizarse es la siguiente: (map (\p -> (transiciona afne p a)) qs) función la cual se encarga de realizar la transición de los estados que forman parte de la cerradura con el simbolo correspondiente del alfabeto
-- Posteriormente al resultado se le hará un concat para hacer una lista de estados y finalmente eliminar los duplicados de esta lista de estados.
-- después de esto se realiza el map anidado, el cual se encargará de que a cada uno de los estados resultantes buscar la cerradura epsilon correspendiente. Eso se realiza con la siguiente parte de la función (nub (concat ( map (\x -> getEpsilonClosure cerradurasEpsilon x) [parteAnterior] )) ) a este resultado es al que se le aplica un concat para convertirlo en una sola lista y un nub para quitar repeticiones.
-- UNa vez hecho esto se realiza un map para cada uno de los elementos del alfabeto del afne se realizará lo anterior, elementos los cuales la salida será formateada como solicita la estructura DeltaE. esto se realiza en la siguiente función (map (\a -> (q,a, [partesAnteriores])) (alfabetoE afne) )
-- Esto será realizado para cada estado devuelto en nuestra cerradura de Epsilon, pensando en no tener que capturar la cerradura dado el estado, porque al iterar sobre estos pares ya tenemos la cerradura y el estado del cual vienen.
-- Finalmente filtraremos estas transiciones resultantes de tal forma que solo nos quedamos con las que no son vacias o no se componen de epsilon. 
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

