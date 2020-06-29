{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Module    :  Control.Carrier.Parser
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Parser effect carriers and evaluation.
--
-- @since 0.1.0.0

module Control.Carrier.Parser
  ( -- * Parser Carrier
    ParserC(..)
    -- ** Parser runners
  , runParser, runParserWithFile, runParserWith
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Effect.Throw
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Algebra
import           Data.Coerce
import           Data.Parser.Err
import           Data.Parser.Input
import           Data.Parser.Notice
import           Data.Text.Prettyprint.Doc
import           Prelude                   hiding (head)

-- | Evaluates a parser effect.
--
-- @since 0.1.0.0
runParser
  :: forall s m a b
  .  (Input s -> a -> m b)
  -> (Err s -> m b)
  -> Input s
  -> ParserC s m a
  -> m b
runParser leaf fail input (ParserC m) = m leaf fail input
{-# INLINE runParser #-}

-- | Evaluate a parser from a file path.
--
-- @since 0.1.0.0
runParserWithFile
  :: (MonadIO m, Has (Throw Notice) sig m)
  => FilePath
  -> ParserC String m a
  -> m a
runParserWithFile path parser = do
  input <- liftIO (readFile path)
  runParserWith path (Input mempty mempty input) parser
{-# INLINE runParserWithFile #-}

-- | @since 0.1.0.0
runParserWith
  :: Has (Throw Notice) sig m
  => FilePath
  -> Input s
  -> ParserC s m a
  -> m a
runParserWith fp input = runParser (const pure) (throwError . errToNotice fp) input
{-# INLINE runParserWith #-}

-- | The carrier of a church-encoded parser effect.
--
-- @since 0.1.0.0
newtype ParserC s m a = ParserC
  { runParserC
    :: forall r
    .  (Input s -> a -> m r)
    -> (Err s -> m r)
    -> Input s
    -> m r
  }
  deriving Functor

-- | @since 0.1.0.0
instance Applicative (ParserC s m) where
  pure a = ParserC $ \ k _ s -> k s a
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad (ParserC s m) where
  ParserC m >>= f = ParserC $ \ leaf fail ->
    m (\ inputs ->
      runParser leaf fail inputs . f) fail
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance Alternative (ParserC s m) where
  empty = ParserC (\_ fail input -> fail (Err input Nothing mempty))
  {-# INLINE empty #-}

  ParserC l <|> ParserC r = ParserC (\leaf fail input ->
    l leaf (\err1 ->
      r leaf (\err2 ->
        fail err1
          { errReason   = err1^.reason'   <|> err2^.reason'
          , errExpected = err2^.expected' <>  err1^.expected'
          }) input) input)
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance MonadPlus (ParserC s m)

-- | @since 0.1.0.0
instance MonadTrans (ParserC s) where
  lift m = ParserC $ \leaf _ inputs -> m >>= leaf inputs
  {-# INLINE lift #-}

-- | @since 0.1.0.0
instance Semigroup k => Semigroup (ParserC s m k) where
  (<>) = liftA2 (<>)

-- | @since 0.1.0.0
instance Monoid k => Monoid (ParserC s m k) where
  mempty = pure mempty

-- | @since 0.1.0.0
instance MonadIO m => MonadIO (ParserC s m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

-- | @since 0.1.0.0
type IsParseable f =
  ( Show            (f Char)
  , Monoid          (f Char)
  , HasAdvance      (f Char)
  , RCoalgebra Maybe f Char
  , FCoalgebra       f Char
  )

-- | @since 0.1.0.0
instance (IsParseable f, Algebra sig m) => Algebra (Parser :+: sig) (ParserC (f Char) m) where
  alg hdl sig ctx = ParserC $ \ leaf fail inputs -> case sig of
    L (Satisfy p f) -> case head (inputs^.input') of
      Just c | Just x <- p c -> runParser leaf fail (advanceInput inputs) (hdl . (<$ ctx) . f $ x)
             | otherwise     -> throwUnexpected fail (singleton c)
      Nothing                -> throwEndOfInput fail

    L (Unexpected msg parser) -> runParser leaf fail' inputs (hdl (parser <$ ctx))
      where fail' err = fail err { errExpected = singleton msg }

    R other -> thread (dst inputs ~<~ hdl) other (pure ctx)
      >>= run . runParser (coerce leaf) (coerce fail) inputs
  {-# INLINE alg #-}

dst :: Applicative m => Input s -> ParserC s Identity (ParserC s m a) -> m (ParserC s Identity a)
dst inputs = run . runParser distParser (pure . cutfailk) inputs
{-# INLINE dst #-}

distParser :: Applicative m => Input s -> ParserC s m a -> Identity (m (ParserC s Identity a))
distParser inputs parser = return (runParser (const (pure . pure)) cutfailk inputs parser)
{-# INLINE distParser #-}

cutfailk :: Applicative m => Err s -> m (ParserC s Identity a)
cutfailk err = pure (ParserC (\ _ fail _ -> fail err))
{-# INLINE cutfailk #-}

throwUnexpected :: (Show s, Monoid s) => (Err s -> m r) -> s -> m r
throwUnexpected fail x = fail (emitPlainErr (pretty "unexpected " <> pretty (show x)))
{-# INLINE throwUnexpected #-}

throwEndOfInput :: Monoid s => (Err s -> m r) -> m r
throwEndOfInput fail = fail (emitPlainErr (pretty "unexpected end of input."))
{-# INLINE throwEndOfInput #-}
