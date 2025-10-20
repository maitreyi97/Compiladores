module MDD.MDD where

import Regex.Regex
import Regex.SpecReader
import AFD.AFD
import AFNE.AFNE
import Data.List

type EstadoMDD = String
type Simbolo = Char

data MDD = MDD {
    estadosMDD :: [EstadoMDD],                   
    alfabetoMDD :: [Simbolo],                       
    transicionesMDD :: [(EstadoMDD, Simbolo, EstadoMDD)],  
    estadoInicialMDD :: EstadoMDD,                
    estadosFinalesMDD :: [EstadoMDD],             
    funcionMu :: [(EstadoMDD, Categoria)],        
    simboloError :: String                        
} deriving (Show)

-- 
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
nombreCategoria :: Categoria -> String
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

