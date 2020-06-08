{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

-- |
--
-- @since 0.1.0.0

module Control.Effect.Satisfy where

import           Control.Algebra
import           Control.Applicative
import           Data.Kind

data Satisfy s (m :: Type -> Type) k where
  Satisfy :: (s -> Bool) -> (s -> k) -> Satisfy s m k
  Peek    :: Satisfy s m s

satisfy :: Has (Satisfy s) sig m => (s -> Bool) -> (s -> k) -> m k
satisfy p f = send (Satisfy p f)
{-# INLINABLE satisfy #-}

-- | Retrieve the head of a list-like environment value.
--
-- @
-- runParser (c : cs) ('peek' '>>=' k) = runParser a (k c)
-- runParser []       ('peek' '>>=' k) = empty
-- @
--
-- @since 0.1.0.0
peek :: Has (Satisfy s) sig m => m s
peek = send Peek
{-# INLINE peek #-}

manyWhile :: (Alternative m, Has (Satisfy s) sig m) => (s -> Bool) -> (s -> k) -> m [k]
manyWhile p f = some (satisfy p f)
{-# INLINEABLE manyWhile #-}
