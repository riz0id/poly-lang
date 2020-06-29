{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

-- | Module    :  Parser.Parser
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
  (  tokLParen, tokRParen
  , tokLBracket, tokRBracket
  , tokDot
  , tokSymbol, tokDigit
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Monad
import           Data.Char
import           Data.Text
import           Parser.Token

tokLParen :: Has Parser sig m => m Parenthesis
tokLParen = char '(' >> return LeftParenthesis
{-# INLINE tokLParen #-}

tokRParen :: Has Parser sig m => m Parenthesis
tokRParen = char ')' >> return RightParenthesis
{-# INLINE tokRParen #-}

tokLBracket :: Has Parser sig m => m Bracket
tokLBracket = char '[' >> return LeftBracket
{-# INLINE tokLBracket #-}

tokRBracket :: Has Parser sig m => m Bracket
tokRBracket = char ']' >> return RightBracket
{-# INLINE tokRBracket #-}

tokDot :: Has Parser sig m => m Token
tokDot = char '.' >> return TokenDot
{-# INLINE tokDot #-}

tokEndOfFile :: Has Parser sig m => m Token
tokEndOfFile = char '\0' >> return TokenEOF
{-# INLINE tokEndOfFile #-}

tokDigit :: (Alternative m, Has Parser sig m) => m Number
tokDigit = do
  nats <- pack <$> some (passes isDigit)
  decs <- option $ do
    void (char '.')
    pack <$> some (passes isDigit)
  return $ case decs of
    Just x  -> NumberDecimal nats x
    Nothing -> NumberInteger nats

tokSymbol :: (Alternative m, Has Parser sig m) => m Token
tokSymbol = do
  x  <- passes isAlpha
  xs <- many (passes (\c -> isDigit c || isAlpha c || c == '-'))
  return $ case x : xs of
    "defn" -> TokenKeyword DefnKW
    other  -> TokenSym (pack other)
