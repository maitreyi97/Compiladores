module Lexer.Lexer where

import MDD.MDD
import Regex.Regex
import Regex.SpecReader

data Token 
    = TokenIf
    | TokenElse
    | TokenWhile
    | TokenSkip
    | TokenAsign
    | TokenInt String
    | TokenId String
    | TokenBool String
    | TokenMathOp String
    | TokenBoolOp String
    | TokenDelim String
    | TokenComentario String
    | TokenError String
    deriving (Show, Eq)

mapToken :: (String, String) -> Token
mapToken ("Control", "if")   = TokenIf
mapToken ("Control", "else") = TokenElse
mapToken ("Control", "while") = TokenWhile
mapToken ("Control", "skip")  = TokenSkip
mapToken ("Asign", lex)      = TokenAsign
mapToken ("Enteros", lex)    = TokenInt lex
mapToken ("Id", lex)         = TokenId lex
mapToken ("Bool", lex)       = TokenBool lex
mapToken ("MathOP", lex)     = TokenMathOp lex
mapToken ("BoolOP", lex)     = TokenBoolOp lex
mapToken ("Delim", lex)      = TokenDelim lex
mapToken ("Comentarios", lex) = TokenComentario lex
mapToken (catError, lex)     = TokenError lex 

lexer :: String -> String -> IO [Token]
lexer rutaSpec cadena = do
  categorias <- obtenerCategoria <$> leeSpec rutaSpec
  let mdd = construirMDD categorias
  let tokensString = tokens mdd cadena
  let tokensMapeados = map mapToken tokensString
  return tokensMapeados
