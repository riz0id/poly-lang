{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |

module Parser
  (
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Monad
import           Data.Text             (Text, pack)
import           Parser.Lexer
import           Parser.SExp
import           Parser.Token

parseDefn :: (Alternative m, Has Parser sig m) => m SExp
parseDefn = do
  void tokLParen
  void parseDefnKW
  fnName <- SAtom <$> parseSymbol
  fnArgs <- parseSVec
  fnBody <- parseSList
  void tokRParen
  return (SCons fnName (SCons fnArgs fnBody))

parseDefnKW :: (Alternative m, Has Parser sig m) => m Token
parseDefnKW = tokSymbol >>= \case
  TokenKeyword DefnKW -> return (TokenKeyword DefnKW)
  other               -> undefined

parseSymbol :: (Alternative m, Has Parser sig m) => m Token
parseSymbol = tokSymbol >>= \case
  TokenSym sym -> return (TokenSym sym)
  other        -> undefined

parseSList :: (Alternative m, Has Parser sig m) => m SExp
parseSList = do
  void tokLParen
  result <- manyAtoms
  void tokRParen
  return result

parseSVec :: (Alternative m, Has Parser sig m) => m SExp
parseSVec = do
  void tokLBracket
  result <- manyAtoms
  void tokRParen
  return result

atom :: (Alternative m, Has Parser sig m) => m SExp
atom = SAtom <$> (tokSymbol <|> TokenNum <$> tokDigit)

manyAtoms :: (Alternative m, Has Parser sig m) => m SExp
manyAtoms = many atom >>= return . foldl (SCons) SNil
