{- HLINT ignore "Redundant $" -}

module Language.Test.Calculator where

import Control.Monad ((<=<))
import Data.Functor (($>))
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Data.Text.Lazy.Read (decimal)
import Language.Calculator.Compilation
import Language.Calculator.Interpretation (interpret)
import Language.Calculator.Syntax
import Language.Test.Common
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Test.Tasty.QuickCheck (ioProperty, testProperty)

--------------------------------

spec :: TestTree
spec =
  testGroup "Calculator" $
    [ testGroup "examples" $
        [ testWatModule "x1" dp $
            compile (Literal 42),
          testWatModule "x2" dp $
            compile (Operation Times (Operation Plus (Literal 3) (Literal 4)) (Literal 5))
        ],
      testProperty "compiler-correct" $ \t -> ioProperty $ do
        let m = compile t
        outCompiled <-
          either fail (\(i, rest) -> assertBool "rest of output after int must be empty" (LazyText.null rest) $> i)
            . decimal
            . LazyTextEncoding.decodeUtf8
            <=< interpretWatModule
            $ m
        let outInterpreted = interpret t
        pure $ outInterpreted == outCompiled
    ]
  where
    dp = "asset/golden/Calculator/"
