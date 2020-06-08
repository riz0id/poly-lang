-- | Facilities for testing documentation is fully generated.
--
-- @since 0.1.0.0

module Main
  ( main
  ) where

import System.Environment
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  autogen <- fmap (<> "/build/doctest/autogen") <$> lookupEnv "HASKELL_DIST_DIR"
  doctest (maybe id ((:) . ("-i" <>)) autogen ("-isrc" : "-ilib/fused-effects/src" : "--fast" : if null args then ["src", "lib/fused-effects/src"] else args))
