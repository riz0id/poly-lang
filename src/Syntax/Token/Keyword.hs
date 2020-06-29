{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Syntax.Token.Keyword where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Data.Text.Prettyprint.Doc
import Data.Text (pack)
import           Syntax.Token.Symbol

-- | Token information for Keywords
--
-- @since 0.1.0.0
data Keyword = KwDefn
    deriving (Enum, Eq, Show, Ord)

-- | @since 0.1.0.0
instance Pretty Keyword where
  pretty KwDefn = "'defn'"
  {-# INLINE pretty #-}

-- | Lexes a 'defn' symbol.
--
-- @since 0.1.0.0
lexDefn :: (Alternative m, Has Parser sig m) => m Keyword
lexDefn = lexSymbol >>= \case
  Symbol "defn" -> return KwDefn
  other         -> unexpected (pack (show other)) empty
{-# INLINE lexDefn #-}
