{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

-- |
--
-- @since 0.1.0.0

module Control.Effect.Peek where

import Control.Algebra
import Data.Kind

data Peek s (m :: Type -> Type) k where
