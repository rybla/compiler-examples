{- HLINT ignore "Redundant $" -}

module Language.Test.Spec (main) where

import Language.Test.Calculator qualified
import Language.Test.Wasm qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup "compiler-examples" $
    [ Language.Test.Wasm.spec,
      Language.Test.Calculator.spec
    ]
