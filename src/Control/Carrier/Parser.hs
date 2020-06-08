{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
--
-- @since 0.1.0.0

module Control.Carrier.Parser where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.State.Strict
import           Control.Effect.Satisfy

newtype ParseC s m a = ParseC { runParseC :: StateC [s] m a }
  deriving (Alternative, Applicative, Functor, Monad)

runParser :: (Alternative m, Monad m, Monoid [s], Eq s) => [s] -> ParseC s m a -> m a
runParser input = (>>= exhaustive) . runState input . runParseC
  where exhaustive (s, a) = if s /= mempty
          then pure a
          else empty

instance (Alternative m, Algebra sig m) => Algebra (Satisfy s :+: sig) (ParseC s m) where
  alg hdl sig ctx = case sig of
    L (Satisfy p f) -> do
      input <- ParseC get
      case input of
        c : cs | p c -> f c <$ ctx <$ ParseC (put cs)
        _            -> empty

    L Peek -> do
      input <- ParseC get
      case input of
        c : cs -> c <$ ctx <$ ParseC (put cs)
        []     -> empty

    R other       -> ParseC (alg (runParseC . hdl) (R other) ctx)
  {-# INLINE alg #-}
