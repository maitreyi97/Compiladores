module Main (main) where

import Lexer.Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let specFile = "src/Regex/spec.txt"
    let inputFile = head args 

    inputString <- readFile inputFile

    putStrLn $ "Analizando archivo: " ++ inputFile
    tokens <- lexer specFile inputString

    putStrLn "--- Tokens Generados ---"
    mapM_ print tokens
    putStrLn "------------------------"

-- Para ejecutarlo:
-- stack run -- samples/imp/test01.imp
