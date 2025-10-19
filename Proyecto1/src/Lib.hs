import AFD.AFD
import AFN.AFN


afdParDeAs = AFD
        ["q0", "q1"]
        ['a', 'b']
        "q0"
        [ ("q0", 'a', "q1"),
          ("q1", 'a', "q0"),
          ("q0", 'b', "q0"),
          ("q1", 'b', "q1")]
        ["q0"]
      
  
miAFN = AFN
  ["q0", "q1", "q2"]           -- Estados
  ['a', 'b']                   -- Alfabeto
  "q0"                         -- Estado Inicial
  [ ("q0", 'a', ["q0", "q1"])   -- Transiciones:
  , ("q0", 'b', ["q0"])         -- Si estoy en q0 y veo una 'b', me quedo en q0.
  , ("q1", 'a', ["q1"])         -- Si estoy en q1 y veo otra 'a', reinicio la cuenta de 'a' quedándome en q1.
  , ("q1", 'b', ["q2"])         -- Si estoy en q1 y veo una 'b', completo "ab" y voy a q2.
  , ("q2", 'a', ["q1"])         -- Si ya terminé y veo una 'a', vuelvo a q1.
  , ("q2", 'b', ["q0"])         -- Si ya terminé y veo una 'b', vuelvo a q0.
  ]
  ["q2"] 

miAFNEstados = ["q0", "q1", "q2"]
miAFNAlfabeto = ['a','b']
miAFNEstadoInicial = "q0"
miAFNTransiciones = [ ("q0", 'a', ["q0", "q1"])   -- Transiciones:
  , ("q0", 'b', ["q0"])         -- Si estoy en q0 y veo una 'b', me quedo en q0.
  , ("q1", 'a', ["q1"])         -- Si estoy en q1 y veo otra 'a', reinicio la cuenta de 'a' quedándome en q1.
  , ("q1", 'b', ["q2"])         -- Si estoy en q1 y veo una 'b', completo "ab" y voy a q2.
  , ("q2", 'a', ["q1"])         -- Si ya terminé y veo una 'a', vuelvo a q1.
  , ("q2", 'b', ["q0"])         -- Si ya terminé y veo una 'b', vuelvo a q0.
  ]
miAFNEstadoFinal = ["q2"]
miAFNTransicionesPotencia = transicionesPotencia miAFNTransiciones (conjuntoPotencia miAFNEstados) miAFNAlfabeto

miAFNEstadosAlcanzables = estadosAlcanzables miAFNTransicionesPotencia [[miAFNEstadoInicial]] []
miAFNEstadosFinales = filter (\x -> (or (map (\y -> (y `elem` x)) miAFNEstadoFinal ) )) miAFNEstadosAlcanzables


afdEjemplo = AFD
             ["q0", "q1", "q2", "q3"]
             ['a','b']
             "q0"
             [("q0", 'a', "q1"),
             ("q1", 'a', "q3"),
             ("q0", 'b', "q2"),
             ("q1", 'b', "q3"),
             ("q2", 'a', "q3"),
             ("q2", 'b', "q3"),
             ("q3", 'a', "q3"),
             ("q3", 'b', "q3")]
             ["q3"]

