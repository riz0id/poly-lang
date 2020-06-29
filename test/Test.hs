-- |

import qualified Test.Parser
import           Test.Tasty      as Tasty

main :: IO ()
main = defaultMain $ testGroup "poly-lang-tests"
  [ Test.Parser.testTree
  ]
