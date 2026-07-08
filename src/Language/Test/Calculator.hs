{- HLINT ignore "Redundant $" -}

module Language.Test.Calculator where

import Language.Calculator.Compilation
import Language.Calculator.Syntax
import Language.Test.Common
import Test.Tasty (TestTree, testGroup)

--------------------------------

spec :: TestTree
spec =
  testGroup "Calculator" $
    [ testWat "x1" dp $
        compile (Literal 42),
      testWat "x2" dp $
        compile (Operation Times (Operation Plus (Literal 3) (Literal 4)) (Literal 5))
    ]
  where
    dp = "asset/golden/Calculator/"
