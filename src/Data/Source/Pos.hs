{-# LANGUAGE DeriveGeneric #-}

-- | Module    :  Data.Source.Pos
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Line and column positional information in a source file.
--
-- @since 0.1.0.0

module Data.Source.Pos
  ( Pos(..)
    -- ** Position Lenses
  , HasPos(..)
    -- ** Position Constructors
  , emptyPos
  , moveNewline, moveColumn
  ) where

import           Control.Lens
import           Data.Source.Delta
import           GHC.Generics

-- | Column and line information for a arbitrary source file.
--
-- @since 0.1.0.0
data Pos = Pos
    { posLine   :: {-# UNPACK #-} !Delta
    , posColumn :: {-# UNPACK #-} !Delta
    }
    deriving (Eq, Generic, Ord, Show)

-- | Create a starting position of a file. Note that we index the first column
-- of a file as column 0 for the sake of simplicity.
--
-- @since 0.1.0.1
emptyPos :: Pos
emptyPos = Pos 0 0
{-# INLINE emptyPos #-}

-- | Classy-fields for record types which contain positions.
--
-- @since 0.1.0.0
class HasPos a where
  -- | "Pos" lens
  --
  -- @since 0.1.0.0
  pos' :: Lens' a Pos

  -- | "Pos" lens for accessing the position's line.
  --
  -- @since 0.1.0.0
  line' :: Lens' a Delta
  line' = pos' . line'
  {-# INLINE line' #-}

  -- | "Column" lens for accessing the position's column.
  --
  -- @since 0.1.0.0
  column' :: Lens' a Delta
  column' = pos' . line'
  {-# INLINE column' #-}

-- | @since 0.1.0.0
instance HasPos Pos where
  pos' = id
  {-# INLINE pos' #-}

-- | Increment line and column information with respect to a new line.
--
-- @since 1.0.0.0
moveNewline :: Pos -> Pos
moveNewline p = p { posLine   = 0
                  , posColumn = p^.column' + 1
                  }
{-# INLINE moveNewline #-}

-- | Increment the column information.
--
-- @since 1.0.0.0
moveColumn :: Pos -> Pos
moveColumn p = p & column' +~ 1
{-# INLINE moveColumn #-}
