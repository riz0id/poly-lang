-- |

module Parser.SExp
  ( SExp(..)
  ) where

import Parser.Token

data SExp
  = SCons
    { sConsLeft  :: SExp
    , sConsRight :: SExp
    }
  | SAtom
    { sAtomToken :: Token
    }
  | SNil
  deriving (Eq, Show)

instance Semigroup SExp where
  (<>) = SCons
  {-# INLINE CONLIKE (<>) #-}

instance Monoid SExp where
  mempty = SNil
  {-# INLINE CONLIKE mempty #-}
