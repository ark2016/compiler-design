module Main where

import Parser (parse)

main :: IO ()
main = do
  putStrLn "Enter code to parse:"
  code <- getLine
  case parse code of
    Just prog -> print prog
    Nothing   -> putStrLn "Parse error"
