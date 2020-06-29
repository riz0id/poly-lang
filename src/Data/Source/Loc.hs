{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module    :  Data.Source.Loc
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Poly language tokens.
--
-- Location information for a source file.
--
-- @since 0.1.0.0

module Data.Source.Loc
  ( Loc(..)
  ) where

import           Control.Lens
import           Data.Source.Range
import           Data.Source.Span
import           GHC.Generics
import           Prelude           hiding (span)

-- | Location information containing "span" and "range" over a file.
--
-- @since 0.1.0.0
data Loc = Loc
    { locSpan  :: {-# UNPACK #-} !Span
    , locRange :: {-# UNPACK #-} !Range
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | @since 0.1.0.0
instance Semigroup Loc where
  Loc s1 r1 <> Loc s2 r2 = Loc (s1 <> s2) (r1 <> r2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance HasSpan Loc where
  span' = lens locSpan (\s t -> s { locSpan = t })
  {-# INLINE span' #-}

-- | @since 0.1.0.0
instance HasRange Loc where
  range' = lens locRange (\s t -> s { locRange = t })
  {-# INLINE range' #-}
