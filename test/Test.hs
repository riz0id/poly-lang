-- |

import qualified Test.Lexer
import           Test.Tasty      as Tasty

main :: IO ()
main = defaultMain $ testGroup "poly-lang-tests"
  [ Test.Lexer.testTree
  ]
