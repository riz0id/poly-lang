{-# LANGUAGE OverloadedStrings #-}

-- |

module Test.Parser where

import           Control.Carrier.Parser
import           Control.Monad.IO.Class
import           Data.Algebra
import           Data.Parser.Err
import           Data.Parser.Input
import           Data.Source
import           Syntax.Parse
import           Syntax.SExp
import           Syntax.Token
import qualified Test.Tasty             as Tasty
import           Test.Tasty.HUnit

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "Parser.Parse"
  [ testCase "Parser.Parse.parseAtom"           test'parseAtom'
  , testCase "Parser.Parse.parseVector - n+1"   test'parseVector'
  , testCase "Parser.Parse.parseVector - n=0"   test'parseVector'Empty
  , testCase "Parser.Parse.parseSExp   - n+1"   test'parseSExp'
  , testCase "Parser.Parse.parseSExp   - n=0"   test'parseSExp'Empty
  , testCase "Parser.Parse.parseSExp   - Embed" test'parseSExp'Embedded
  , testCase "Parser.Parse.parseDefn"           test'parseDefn'
  ]

test'parseDefn' :: Assertion
test'parseDefn' = do
  result <- runParserTestWithFile "./test/Programs/Add.ply" parseDefn
  result @?= mempty

test'parseSExp' :: Assertion
test'parseSExp' = do
  result <- runParserTest "( a b )" parseSExp
  result @?= SCons "a" (SCons "b" SNil mempty) mempty

test'parseSExp'Embedded :: Assertion
test'parseSExp'Embedded = do
  result <- runParserTest "(a (a))" parseSExp
  result @?= SCons "a" (SCons (singleton "a") SNil mempty) mempty

test'parseSExp'Empty :: Assertion
test'parseSExp'Empty = do
  result <- runParserTest "()" parseSExp
  result @?= mempty

test'parseVector' :: Assertion
test'parseVector' = do
  result <- runParserTest "[ a b ]" parseVector
  result @?= SCons "a" (SCons "b" SNil mempty) mempty

test'parseVector'Empty :: Assertion
test'parseVector'Empty = do
  result <- runParserTest "[]" parseVector
  result @?= mempty

test'parseAtom' :: Assertion
test'parseAtom' = do
  result <- runParserTest "la-la1" parseAtom
  result @?= "la-la1"

runParserTest
  :: Applicative m
  => String
  -> ParserC String m (SExp Token)
  -> m (SExp Token)
runParserTest input =
  runParser (const pure) (error . show . errExpected) (Input emptyPos mempty input)

runParserTestWithFile
  :: MonadIO m
  => FilePath
  -> ParserC String m a
  -> m a
runParserTestWithFile path parser = do
  input <- liftIO (readFile path)
  runParserTestWith (Input emptyPos mempty input) parser

runParserTestWith
  :: (Show s, Applicative m)
  => Input s
  -> ParserC s m a
  -> m a
runParserTestWith input = runParser (const pure) (error . show) input
