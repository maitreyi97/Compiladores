{
module RegexParser where 
import Regex.RegEx
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
       -- Definimos el Lexer de texto a token para luego transformar a algo basado en reglas.
       parseError :: [Token] -> a
       parseError _ = error "Parse error"

       data Token = TokenLParen
                    | TokenRParen
                    | TokenPlus
                    | TokenRep
                    | TokenOr
                    | TokenLiteral Char
                    | TokenEOF
                    deriving Show 
       lexer :: String -> [Token]
       lexer [] = [TokenEOF]
       lexer ('\\':'(':cs) = TokenLiteral '(' : lexer cs
       lexer ('\\':')':cs) = TokenLiteral ')' : lexer cs
       lexer ('(':cs) = TokenLParen : lexer cs
       lexer (')':cs) = TokenRParen : lexer cs
       lexer ('+':cs) = TokenPlus : lexer cs 
       lexer ('*':cs) = TokenRep : lexer cs 
       lexer ('|':cs) = TokenOr : lexer cs
       lexer (c:cs) = TokenLiteral c : lexer cs
}