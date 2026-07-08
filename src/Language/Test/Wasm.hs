{- HLINT ignore "Redundant $" -}

module Language.Test.Wasm where

import Language.Test.Wasm.Wat qualified
import Test.Tasty (TestTree, testGroup)

spec :: TestTree
spec =
  testGroup "Wasm" $
    [ Language.Test.Wasm.Wat.spec
    ]
