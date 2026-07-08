{- HLINT ignore "Redundant $" -}

module Language.Test.Spec (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup "compiler-examples" $
    []
