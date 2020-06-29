-- |

module Syntax.Token
  ( Token(..)

    -- * Re-exports
  , module Syntax.Token.Symbol
  , module Syntax.Token.Keyword
  ) where

import           GHC.Exts
import           Syntax.Token.Keyword
import           Syntax.Token.Symbol

data Token = TokenSymbol Symbol
    | TokenKeyword Keyword
    deriving (Eq, Show)

-- | @since 0.1.0.0
instance IsString Token where
  fromString = TokenSymbol . fromString
