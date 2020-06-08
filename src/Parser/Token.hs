{-# LANGUAGE DeriveGeneric #-}

-- | Module    :  Parser.Token
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Poly language tokens.
--
-- @since 0.1.0.0

module Parser.Token
  ( Token(..), Paren(..)
  ) where

import GHC.Generics

-- | Token information
--
-- @since 0.1.0.0
data Token
  = TokParen Paren
  | TokSym String
  | TokNum String
  | TokDot
  | TokEOF
  deriving (Eq, Ord, Generic, Show)

-- | Parenthesis
--
-- @since 0.1.0.0
data Paren
  = ParenLeft
  | ParenRight
  deriving (Eq, Enum, Ord, Generic, Show)
