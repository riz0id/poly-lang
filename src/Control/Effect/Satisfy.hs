{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module    :  Control.Effect.Satisfy
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- A set of effects for parsing combinators.
--
-- @since 0.1.0.0

module Control.Effect.Satisfy
  ( Satisfy(..), satisfy, satisfyMaybe, peek, manyWhile
  ) where

import           Control.Algebra
import           Control.Applicative
import           Data.Kind

-- | Satisfy effect is for interacting with a 'ParserC' carrier.
--
-- @since 0.1.0.0
data Satisfy s (m :: Type -> Type) k where
  Satisfy :: (s -> Bool) -> (s -> k) -> Satisfy s m k
  Peek    :: Satisfy s m (Maybe s)

-- | \(\mathcal{O}(1)\). Expect the first element of a 'ParserC' source to
-- satisfy a predicate @p@. Returns 'empty' if it does not.
--
-- @
-- runParser (c : cs) ('satisfy' p f)
--   | p c       = runParser (f c) cs
--   | otherwise = 'empty'
-- runParser [] ('satisfy' p f) = 'empty'
-- @
--
-- @since 0.1.0.0
satisfy :: Has (Satisfy s) sig m => (s -> Bool) -> (s -> k) -> m k
satisfy p f = send (Satisfy p f)
{-# INLINABLE satisfy #-}

satisfyMaybe :: forall s sig m k. Has (Satisfy s) sig m
             => (s -> Bool) -> (s -> k) -> m (Maybe k)
satisfyMaybe p f =  peek @s >>= \case
  Just _  -> satisfy p f >>= return . Just
  Nothing -> return Nothing

-- | \(\mathcal{O}(1)\). Extract the first element of 'ParserC' source, which
-- must be non-empty.
--
-- @
-- runParser (c : cs) 'peek' = runParser a (k c)
-- runParser []       'peek' = 'empty'
-- @
--
-- @since 0.1.0.0
peek :: Has (Satisfy s) sig m => m (Maybe s)
peek = send Peek
{-# INLINE peek #-}

-- | \(\mathcal{O}(n)\). Takes the head of a list while a predicate is
-- satisfied.
--
-- @since 0.1.0.0
manyWhile :: (Alternative m, Has (Satisfy s) sig m) => (s -> Bool) -> (s -> k) -> m [k]
manyWhile p f = peek >>= \case
  Just c  | p c       -> liftA2 (:) (return (f c)) (manyWhile p f)
  _       | otherwise -> return []
{-# INLINEABLE manyWhile #-}
