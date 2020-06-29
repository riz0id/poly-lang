-- |

module Syntax.Parse.Lexer where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Data.Char
import           Data.Text (pack, unpack)
import           Syntax.Token

lexSymbol :: (Alternative m, Has Parser sig m) => m Symbol
lexSymbol = do
  x  <- passes isAlpha
  xs <- many $ passes $ \c ->
    isDigit c || isAlpha c || c == '-'
  return . Symbol . pack $ x : xs
