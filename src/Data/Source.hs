{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module    :  Data.Source
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

module Data.Source
  ( -- * Operations
    advanceStart, advanceEnd
    -- * Locations
  , Loc(..)
    -- ** Location Classes
  , HasSpan(..), HasRange(..)
    -- * Positions
  , Pos(..), emptyPos
    -- ** Position Classes
  , HasPos(..)
    -- * Pos Operations
  , moveNewline, moveColumn
    -- * Ranges
  , Range(..), rangeLength
    -- * Spans
  , Span(..)
    -- ** Span Classes
  , HasInterval(..)
    -- * Deltas
  , Delta(..)
    -- ** Delta Classes
  , HasDelta(..)
  ) where

import           Control.Lens
import           Data.Source.Delta
import           Data.Source.Loc
import           Data.Source.Pos
import           Data.Source.Range
import           Data.Source.Span
import           Data.String       (IsString (..))
import           GHC.Generics
import           Prelude           hiding (drop, length, null, take)

-- | Advance the starting position on an arbitrary interval.
--
-- @since 0.1.0.0
advanceStart :: (Num b, HasInterval a b) => a -> a
advanceStart x = x & start' +~ 1

-- | Advance the ending position on an arbitrary interval.
--
-- @since 0.1.0.0
advanceEnd :: (Num b, HasInterval a b) => a -> a
advanceEnd x = x & end' +~ 1
