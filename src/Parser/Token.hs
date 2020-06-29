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
  ( Token(..), isSym
  , Keyword(..)
  , Bracket(..)
  , Parenthesis(..), isLeftParen, isRightParen
  , Number(..), isNum
  ) where

import           Data.Text
import           GHC.Generics

-- | Token information
--
-- @since 0.1.0.0
data Token
  = TokenParen Parenthesis
  | TokenBracket Bracket
  | TokenKeyword Keyword
  | TokenSym Text
  | TokenNum Number
  | TokenDot
  | TokenEOF
  deriving (Eq, Ord, Generic, Show)

isSym :: Token -> Bool
isSym (TokenSym _) = True
isSym _            = False
{-# INLINE isSym #-}

-- | Keywords
--
-- @since 0.1.0.0
data Keyword
  = DefnKW
  deriving (Eq, Enum, Ord, Show)

-- | Numbers
--
-- @since 0.1.0.0
data Number
  = NumberDecimal Text Text
  | NumberInteger Text
  deriving (Eq, Ord, Show)

isNum :: Token -> Bool
isNum (TokenNum _) = True
isNum _            = False
{-# INLINE isNum #-}

-- | Brackets
data Bracket
  = RightBracket
  | LeftBracket
  deriving (Eq, Enum, Ord, Show)

-- | Parenthesis
--
-- @since 0.1.0.0
data Parenthesis
  = RightParenthesis
  | LeftParenthesis
  deriving (Eq, Enum, Ord, Generic, Show)

isLeftParen :: Token -> Bool
isLeftParen (TokenParen LeftParenthesis) = True
isLeftParen _                            = False
{-# INLINE isLeftParen #-}

isRightParen :: Token -> Bool
isRightParen (TokenParen RightParenthesis) = True
isRightParen _                             = False
{-# INLINE isRightParen #-}
