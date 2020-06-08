-- |

module Test.Lexer (testTree) where

import           Control.Monad
import           Hedgehog            hiding (Range)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Parser.Lexer
import           Parser.Token
import qualified Test.Tasty          as Tasty
import           Test.Tasty.HUnit

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Parser.Lexer"
  [ testCase "Lexer.Symbol" testSymbol
  , Tasty.testGroup "Lexer.Parenthesis"
    [ testCase "Lexer.leftParen:  Lexing '('" testLeftParen
    , testCase "Lexer.rightParen: Lexing ')'" testRightParen
    ]
  ]

-- | Randomized tests on symbols to verifying lexical analysis on them.
--
-- @since 0.1.0.0
testSymbol :: Assertion
testSymbol = replicateM_ 100 $ do
  randSymbol <- Gen.sample genSymbol
  result     <- stepLexer randSymbol (symbol)
  snd result @?= TokSym randSymbol

-- | Generates a random symbol adherent to what we know the lisp syntax for a
-- symbol is.
--
-- @
-- symbol ::= (A-Z, a-Z)[A-Z,a-z,0-9,-]
-- @
--
-- @since 0.1.0.0
genSymbol :: MonadGen m => m String
genSymbol = do
  symHead <- Gen.alpha
  symTail <- Gen.string (Range.constant 0 256) . Gen.element
          $  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          ++ "abcdefghijklmnopqrstuvwxyz"
          ++ "1234567890"
          ++ "-"
  return (symHead : symTail)


-- | Tests that out what is satisfied for a left parenthesis is boxed correctly.
--
-- @since 0.1.0.0
testLeftParen :: Assertion
testLeftParen = do
  result <- stepLexer "(" leftParen
  snd result @?= TokParen ParenLeft

-- | Tests that out what is satisfied for a right parenthesis is boxed correctly.
--
-- @since 0.1.0.0
testRightParen :: Assertion
testRightParen = do
  result <- stepLexer ")" rightParen
  snd result @?= TokParen ParenRight
