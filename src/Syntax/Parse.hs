{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- |

module Syntax.Parse where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Monad
import           Data.Algebra
import           Syntax.SExp
import           Syntax.SExp.Adornments
import           Syntax.Token

parseDefn :: (Alternative m, Has Parser sig m) => m (SExp Token)
parseDefn = do
  void (char '(' *> skipSpace *> lexDefn <* skipSpace)
  name <- (SAtom . TokenSymbol <$> lexSymbol)  <* skipSpace
  args <- (parseVector <!> "expected vector.") <* skipSpace
  list <- parseSExp   <* skipSpace
  void (char ')' <!> "expected a ')' to close the function")
  return $ SCons name
    (SCons args list mempty)
    (singleton AdornFunction)

parseSExp :: (Alternative m, Has Parser sig m) => m (SExp Token)
parseSExp = do
  void (char '(')
  xs <- many $ do
    void skipSpace
    result <- (parseAtom <|> parseSExp) <!> "expected symbol, number or list."
    void skipSpace
    return result
  void (char ')')
  return (join $ transfer xs)

parseVector :: (Alternative m, Has Parser sig m) => m (SExp Token)
parseVector = do
  void (char '[' <* skipSpace)
  xs <- manyAtoms
  void (char ']')
  return xs
 
parseAtom :: (Alternative m, Has Parser sig m) => m (SExp Token)
parseAtom = SAtom
  <$> (TokenSymbol  <$> lexSymbol)

manyAtoms :: (Alternative m, Has Parser sig m) => m (SExp Token)
manyAtoms = fmap (join . transfer) $ many (parseAtom <* skipSpace)
