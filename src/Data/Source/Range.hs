{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module    :  Data.Source.Range
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Range over a source file, this is different from a "Span" in that "Range"
-- only carries two indicies for an interval over a source.
--
--
-- @since 0.1.0.0

module Data.Source.Range
  ( Range(..)
    -- ** Range Classes
  , HasRange(..)
    -- ** Range Operations
  , rangeLength
  ) where

import           Control.Lens
import           Data.Source.Delta
import           Data.Source.Span (HasInterval (..))
import           GHC.Generics

-- | An index range over a source file.
--
-- @since 0.1.0.0
data Range = Range
    { rangeStart :: {-# UNPACK #-} !Delta -- ^ The start of the "Range".
    , rangeEnd   :: {-# UNPACK #-} !Delta -- ^ The end of the "Range".
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | @since 0.1.0.0
instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.1
instance Monoid Range where
  mempty = Range 0 0
  {-# INLINE mempty #-}

-- | Classy-fields for things with "Range"s.
class HasRange a where
  -- | Access the range from an arbitrary type.
  range' :: Lens' a Range

-- | @since 0.1.0.0
instance HasRange Range where
  range' =  lens id (\_ t -> t)

-- | @since 0.1.0.0
instance HasInterval Range Delta where
  start' = lens rangeStart (\s t -> s { rangeStart = t })
  {-# INLINE start' #-}

  end' = lens rangeEnd (\s t -> s { rangeEnd = t })
  {-# INLINE end' #-}

-- | Takes the length between beginning("start'") and end ("end'") of the range.
--
-- @since 0.1.0.0
rangeLength :: Range -> Delta
rangeLength range = rangeEnd range - rangeStart range
