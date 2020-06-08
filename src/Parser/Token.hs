{-# LANGUAGE DeriveGeneric #-}

-- |
--
-- @since 0.1.0.0

module Parser.Token
  ( Token(..), Paren(..)
  ) where

import GHC.Generics

-- |
--
-- @since 0.1.0.0
data Token
  = TokParen Paren
  | TokSym String
  deriving (Eq, Ord, Generic, Show)

-- |
--
-- @since 0.1.0.0
data Paren
  = ParenLeft
  | ParenRight
  deriving (Eq, Enum, Ord, Generic, Show)
