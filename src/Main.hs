module Main where

import Parser.Lexer

main :: IO ()
main = do
  test <- runLexer "alalalal\0" lexer
  print test
