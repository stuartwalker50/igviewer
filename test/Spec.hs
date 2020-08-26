import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Data.Sequence as S

import Lib
import Database

main :: IO ()
main = do
  defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit tests"
  []