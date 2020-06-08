-- |
--
-- @since 0.1.0.0

module Parser.Lexer
  ( runLexer, lexer
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.Parser
import           Control.Effect.Satisfy
import           Control.Monad
import           Data.Char
import           Parser.Token

-- |
--
-- @since 0.1.0.0
type LexerC = ParseC Char

-- |
--
-- @since 0.1.0.0
type Lexeme = Satisfy Char

-- |
--
-- @since 0.1.0.0
runLexer :: (Alternative m, Monad m) => String -> LexerC m Token -> m Token
runLexer = runParser

-- |
--
-- @since 0.1.0.0
lexer :: (Alternative m, Monad m, Has Lexeme sig m) => m Token
lexer = symbol <|> leftParen <|> rightParen

-- | Consume a symbol from the input.
--
-- @since 0.1.0.0
symbol :: (Alternative m, Has Lexeme sig m) => m Token
symbol = do
  peek >>= guard . isAlpha
  TokSym <$> manyWhile (\x -> isAlpha x || isDigit x || x == '-') id

-- |
--
-- @since 0.1.0.0
char :: Has Lexeme sig m => Char -> (Char -> Token) -> m Token
char c = satisfy (c ==)

-- | Consumes a left parenthesis.
--
-- @since 0.1.0.0
leftParen :: Has Lexeme sig m => m Token
leftParen = char '(' (const (TokParen ParenLeft))

-- | Consumes a right parenthesis.
--
-- @since 0.1.0.0
rightParen :: Has Lexeme sig m => m Token
rightParen = char ')' (const (TokParen ParenRight))
