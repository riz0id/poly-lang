-- |

module Syntax.Token.Symbol
  ( Symbol(..), lexSymbol
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Data.Char
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           GHC.Exts

-- | Token information for Symbols
--
-- @since 0.1.0.0
data Symbol = Symbol Text
    deriving (Eq, Show)

-- | @since 0.1.0.0
instance Pretty Symbol where
  pretty (Symbol x) = pretty x

-- | @since 0.1.0.0
instance Semigroup Symbol where
  Symbol t1 <> Symbol t2 = Symbol (t1 <> t2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Symbol where
  mempty = Symbol mempty

-- | @since 0.1.0.0
instance IsString Symbol where
  fromString = Symbol . pack

-- | Lexes an arbitrary symbol given the syntax:
--
-- >> symbol ::= (a-z,A-Z)[a-z,A-Z,0-9,-]*
--
-- @since 0.1.0.0
lexSymbol :: (Alternative m, Has Parser sig m) => m Symbol
lexSymbol = do
  x  <- passes isAlpha
  xs <- many . passes $ \c ->
    isDigit c || isAlpha c || c == '-'
  Symbol <$> return (pack (x : xs))
