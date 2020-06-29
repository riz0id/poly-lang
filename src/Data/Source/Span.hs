{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Module    :  Data.Source.Span
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Spans over a source file.
--
-- @since 0.1.0.0

module Data.Source.Span
  ( Span(..), HasInterval(..), HasSpan(..)
  ) where

import           Control.Lens
import           Data.Source.Pos
import           GHC.Generics

-- | A span is the space between to "Pos", a beginning "Pos" and a ending "Pos".
--
-- @since 0.1.0.0
data Span = Span
    { spanStart :: {-# UNPACK #-} !Pos
    , spanEnd   :: {-# UNPACK #-} !Pos
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | @since 0.1.0.0
instance Semigroup Span where
  Span s1 e1 <> Span s2 e2 = Span (min s1 s2) (max e1 e2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Span where
  mempty = Span (Pos 0 0) (Pos 0 0)

-- | Classy-fields for record types which contain "Span"
--
-- @since 0.1.0.0
class HasSpan a where
  -- | "Span" lens
  --
  -- @since 0.1.0.0
  span' :: Lens' a Span

-- | Classy-fields for things with finite intervals.
--
-- @since 0.1.0.0
class HasInterval a b | a -> b where
  -- | The beginning of a Span-like finite interval.
  --
  -- @since 0.1.0.0
  start' :: Lens' a b

  -- | The end of a Span-like finite interval.
  --
  -- @since 0.1.0.0
  end'   :: Lens' a b

-- | @since 0.1.0.0
instance HasInterval Span Pos where
  start' = lens spanStart (\s t -> s { spanStart = t })
  {-# INLINE start' #-}

  end' = lens spanEnd (\s t -> s { spanEnd = t })
  {-# INLINE end' #-}
