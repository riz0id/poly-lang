{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Module    :  Data.Parser.Input
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Files and strings types with positional information.
--
-- @since 0.1.0.0

module Data.Parser.Input
  ( Input(..)
    -- ** Input Classes
  , HasAdvance(..)
    -- ** Input Lenses
  , input'
    -- ** Input Operations
  ) where

import           Control.Lens         (Lens', lens, (%~), (&), (.~), (^.))
import           Data.Algebra         as Algebra
import           Data.Functor.Product
import           Data.Source
import           Data.Text            (Text)

-- | Parser "Input" information.
--
-- @since 0.1.0.0
data Input s = Input
    { inputSpan  :: {-# UNPACK #-} !Span
    , inputRange :: {-# UNPACK #-} !Range
    , inputData  :: !s
    }
    deriving (Eq, Ord, Show)

instance (RCoalgebra Maybe f a) => RCoalgebra Maybe Input (f a) where
  rcomult (Input s r (Algebra.uncons -> (Just x,  xs))) = Pair (Just x) (Input s r xs)
  rcomult (Input s r (Algebra.uncons -> (Nothing, xs))) = Pair Nothing  (Input s r xs)


-- | @since 0.1.0.0
instance Semigroup s => Semigroup (Input s) where
  Input s1 r1 d1 <> Input s2 r2 d2 = Input (s1 <> s2) (r1 <> r2) (d1 <> d2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid s => Monoid (Input s) where
  mempty = Input mempty mempty mempty
  {-# INLINE mempty #-}

-- | @since 0.1.0.0
instance HasSpan (Input s) where
  span' = lens inputSpan (\s t -> s { inputSpan = t })
  {-# INLINE span' #-}

-- | @since 0.1.0.0
instance HasRange (Input s) where
  range' = lens inputRange (\s t -> s { inputRange = t })
  {-# INLINE range' #-}

-- | @since 0.1.0.0
instance HasInterval (Input s) Delta where
  start' = lens (rangeStart . inputRange) (\(Input p r s) t -> Input p (r { rangeStart = t}) s)
  {-# INLINE start' #-}

  end'   = lens (rangeEnd   . inputRange) (\(Input p r s) t -> Input p (r { rangeEnd   = t}) s)
  {-# INLINE end' #-}

-- | 'str' lens for inputs.
--
-- @since 0.1.0.0
input' :: Lens' (Input s) s
input' = lens inputData (\s t -> s { inputData = t })
{-# INLINE input' #-}

-- | Whether a input can be advanced in a normal way. This includes anything
-- that is nominally a recursive type: Lists, Text, etc...
--
-- @since 0.1.0.0
class HasAdvance a where
  advanceInput :: Input a -> Input a

-- | @since 0.1.0.0
instance HasAdvance String where
  advanceInput i = i
    { inputRange = advanceEnd (i^.range')
    , inputData = case inputData i of
        c : cs -> cs
        []     -> []
    }

-- | @since 0.1.0.0
instance HasAdvance Text where
  {-advanceInput (Input p r txt) = case uncons txt of
    Just ('\n', ts) -> Input (p & end' %~ moveNewline) (advanceEnd r) ts
    Just (_   , ts) -> Input (p & end' %~ moveColumn)  (advanceEnd r) ts
    Nothing         -> Input p r mempty
  {-# INLINE advanceInput #-}
-}
