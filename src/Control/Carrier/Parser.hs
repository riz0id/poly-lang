{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Module    :  Control.Carrier.Parser
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Parser carrier for the 'Satisfy' effect, it wraps the 'StateC' carrier adding
-- stream-like consumption to a strict inner state.
--
-- @since 0.1.0.0

module Control.Carrier.Parser
  ( ParseC(..), runParser, stepParser
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.State.Strict
import           Control.Effect.Satisfy

-- | Carrier for a parser.
--
-- @since 0.1.0.0
newtype ParseC s m a = ParseC { runParseC :: StateC [s] m a }
  deriving (Alternative, Applicative, Functor, Monad)

-- | Run a 'Satisfy' effect starting from the passed monoid @[s]@
--
-- @since 0.1.0.0
runParser :: (Alternative m, Monad m, Eq s) => [s] -> ParseC s m a -> m a
runParser input = (>>= exhaustive) . runState input . runParseC
  where exhaustive (s, a) = if s /= mempty
          then pure a
          else empty

-- | Run a 'Satisfy' effect consuming only as much of the internal state as the
-- parsing function @ParseC s m a@ expects, leaving the rest. As opposed to
-- 'runParser' which consumes as much input as is satisfied by the 'Satisfy'
-- effect.
--
-- @since 0.1.0.0
stepParser :: [s] -> ParseC s m a -> m ([s], a)
stepParser input = runState input . runParseC

-- | @since 0.1.0.0
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
        c : cs -> Just c <$ ctx <$ ParseC (put cs)
        []     -> ParseC (StateC (\_ -> return ([], Nothing <$ ctx)))

    R other       -> ParseC (alg (runParseC . hdl) (R other) ctx)
  {-# INLINE alg #-}
