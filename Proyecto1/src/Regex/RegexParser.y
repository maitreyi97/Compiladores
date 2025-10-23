{
module Regex.RegexParser where 
import Regex.Regex
}

%name parseRegex
%tokentype { Token }
%error { parseError }

-- Definimos tokens usados en las expresiones regulares.
%token 
      '(' {TokenLParen}
      ')' {TokenRParen}
      '+' {TokenPlus}
      '*' {TokenRep}
      '|' {TokenOr}
      literal {TokenLiteral $$}
      eof {TokenEOF}
%%

-- Definimos las reglas para las expresiones regulares.
Regex         : Disyuncion eof {$1}

Disyuncion    : Concatenacion '|' Disyuncion { Or $1 $3 }
              | Concatenacion                { $1 }

Concatenacion : Repeticion Concatenacion     { Concat $1 $2 }
              | Repeticion                   { $1 }

Repeticion    : Base '*'                     { Star $1 }
              | Base '+'                     { Plus $1 }
              | Base                         { $1 }

Base          : literal                      { Literal $1 }
              | '(' Disyuncion ')'           { $2 }

{
-- Definicmos los tokens en Haskell.
data Token = TokenLParen
                  | TokenRParen
                  | TokenPlus
                  | TokenRep
                  | TokenOr
                  | TokenLiteral Char
                  | TokenEOF
                  deriving Show 
-- Definimos el erroren caso de no enconetrar una expresión válida.
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Definimos el lexer que vamos a usar para las expresiones regulares.
lexer :: String -> [Token]
lexer [] = [TokenEOF]
lexer ('\\':'(':cs) = TokenLiteral '(' : lexer cs
lexer ('\\':')':cs) = TokenLiteral ')' : lexer cs
lexer ('\\':'+':cs) = TokenLiteral '+' : lexer cs
lexer ('\\':'*':cs) = TokenLiteral '*' : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenRep : lexer cs 
lexer ('|':cs) = TokenOr : lexer cs
lexer (c:cs) = TokenLiteral c : lexer cs
}