-- | Module    :  Data.Source.Delta
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- File offset information.
--
-- @since 0.1.0.1

module Data.Source.Delta
  ( -- * Deltas
    Delta(..)
    -- ** Lenses
  , HasDelta(..)
  ) where

import Control.Lens

-- | Delta is a newtype wrapper over offset information in the a file input.
--
-- @since 0.1.0.0
data Delta = Delta
    { unDelta :: {-# UNPACK #-} !Int
    }
    deriving (Eq, Ord, Show)

-- | Additive monoid
--
-- @since 0.1.0.1
instance Semigroup Delta where
  Delta d1 <> Delta d2 = Delta (d1 + d2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.1
instance Monoid Delta where
  mempty = Delta 0
  {-# INLINE mempty #-}

-- | @since 0.1.0.1
instance Num Delta where
  Delta d1 + Delta d2 = Delta (d1 + d2)
  {-# INLINE (+) #-}

  Delta d1 * Delta d2 = Delta (d1 * d2)
  {-# INLINE (*) #-}

  signum (Delta d)    = Delta (signum d)
  {-# INLINE signum #-}

  negate (Delta d)    = Delta (negate d)
  {-# INLINE negate #-}

  abs    (Delta d)    = Delta (abs d)
  {-# INLINE abs #-}

  fromInteger = Delta . fromInteger
  {-# INLINE fromInteger #-}

-- | Class-fields for record types which contain "Delta"
--
-- @since 0.1.0.1
class HasDelta a where
  delta' :: Lens' a Delta
