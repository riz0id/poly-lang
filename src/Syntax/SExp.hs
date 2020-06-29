{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Syntax.SExp
  ( SExp(..)
  ) where

import           Control.Monad
import           Data.Algebra
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Set
import           GHC.Exts
import           Syntax.SExp.Adornments
import           Syntax.Token

data SExp a = SCons (SExp a) (SExp a) (Set Adornment)
    | SAtom a
    | SNil
    deriving (Eq, Show, Functor)

-- | @since 0.1.0.0
instance IsString (SExp Token) where
  fromString = SAtom . TokenSymbol . fromString

-- | @since 0.1.0.0
instance Semigroup (SExp a) where
  x <> y = SCons x y mempty
  {-# INLINE CONLIKE (<>) #-}

-- | @since 0.1.0.0
instance Monoid (SExp a) where
  mempty = SNil
  {-# INLINE CONLIKE mempty #-}

-- | @since 0.1.0.0
instance Applicative SExp where
  pure = SAtom
  {-# INLINE CONLIKE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad SExp where
  (SCons x y o) >>= m = SCons (x >>= m) (y >>= m) o
  (SAtom x)     >>= m = m x
  SNil          >>= _ = SNil
  {-# INLINE (>>=) #-}

-- | This could just as well be @funit = SAtom@ but this is differs semantically
-- from the actualy interpretation of a SExp.
--
-- @since 0.1.0.0
instance FCoalgebra SExp a where
  funit x = SCons (SAtom x) SNil mempty
  {-# INLINE CONLIKE funit #-}

-- | @since 0.1.0.0
instance RAlgebra Identity SExp a where
  rmult (Pair x xs) = SCons (SAtom (runIdentity x)) xs mempty
  {-# INLINE CONLIKE rmult #-}
