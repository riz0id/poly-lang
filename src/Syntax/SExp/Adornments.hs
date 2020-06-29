-- |

module Syntax.SExp.Adornments where

data Adornment
  = AdornFunction
  | AdornList
  | AdornVector
  deriving (Eq, Ord, Show, Enum)
