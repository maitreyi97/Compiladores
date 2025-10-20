module MDD.MDD where
import Regex.Regex
import Regex.SpecReader

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

-- 