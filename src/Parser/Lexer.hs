{-# LANGUAGE LambdaCase #-}
-- | Module    :  Parser.Lexer
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Poly-lang lexer.
--
-- @since 0.1.0.0

module Parser.Lexer
  ( stepLexer, runLexer, lexer
    -- * Combinatorial functions
  , symbol, leftParen, rightParen
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.Parser
import           Control.Effect.Satisfy
import Data.Maybe
import           Control.Monad
import           Data.Char
import           Parser.Token

-- | Lexer over character.
--
-- @since 0.1.0.0
type LexerC = ParseC Char

-- | Satisfy over characters.
--
-- @since 0.1.0.0
type Lexeme = Satisfy Char

-- | @since 0.1.0.0
runLexer :: (Alternative m, Monad m) => String -> LexerC m k -> m k
runLexer = runParser

-- | @since 0.1.0.0
stepLexer :: (Alternative m, Monad m) => String -> LexerC m k -> m (String, k)
stepLexer = stepParser

-- | @since 0.1.0.0
lexer :: (Alternative m, Has Lexeme sig m) => m [Token]
lexer = parens <|> endOfFile

-- | @since 0.1.0
parens :: (Alternative m, Has Lexeme sig m) => m [Token]
parens = leftParen *> many (symbol <|> number <|> dot) <* rightParen

-- | @since 0.1.0.0
symbol :: (Alternative m, Has Lexeme sig m) => m Token
symbol = TokSym <$> manyWhile isSym id

isSym :: Char -> Bool
isSym c = isAlpha c || isDigit c || c == '-'

-- | @since 0.1.0.0
number :: (Alternative m, Has Lexeme sig m) => m Token
number = TokNum <$> manyWhile isDigit id

-- | @since 0.1.0.0
endOfFile :: Has Lexeme sig m => m [Token]
endOfFile = do
  t <- char '\0' (const TokEOF)
  return [t]

-- | @since 0.1.0.0
char :: Has Lexeme sig m => Char -> (Char -> Token) -> m Token
char c = satisfy (c ==)

dot :: Has Lexeme sig m => m Token
dot = char '.' (const TokDot)

-- | @since 0.1.0.0
leftParen :: Has Lexeme sig m => m Token
leftParen = char '(' (const (TokParen ParenLeft))

-- | @since 0.1.0.0
rightParen :: Has Lexeme sig m => m Token
rightParen = char ')' (const (TokParen ParenRight))
