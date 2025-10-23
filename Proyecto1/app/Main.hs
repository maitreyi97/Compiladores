module Main (main) where

import Lexer.Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [specFile, inputFile] -> do 
        inputString <- readFile inputFile
        putStrLn $ "Analizando archivo: " ++ inputFile
        tokens <- lexer specFile inputString

        putStrLn "--- Tokens Generados ---"
        mapM_ print tokens
        putStrLn "------------------------"
      _ -> do
        putStrLn "Argumentos: <Archivo especificación> <Archivo Código>"
