{- HLINT ignore "Redundant $" -}

module Language.Test.Calculator where

import Control.Monad (unless, (<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Data.Text.Lazy.Read (decimal, signed)
import Language.Calculator.Compilation
import Language.Calculator.Interpretation (interpret)
import Language.Calculator.Syntax
import Language.Test.Common
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (idempotentIOProperty, testProperty)

--------------------------------

spec :: TestTree
spec =
  testGroup "Calculator" $
    let dp = "asset/golden/Calculator/"
     in [ testGroup "examples" $
            [ testWatModule "x1" dp [] . compile $
                Literal 42,
              testWatModule "x2" dp [] . compile $
                Operation Times (Operation Plus (Literal 3) (Literal 4)) (Literal 5)
            ],
          testProperty "compiler-correct" $ \t -> idempotentIOProperty $ do
            outCompiled <-
              either (fail . LazyText.unpack) pure <=< runExceptT
                $ either
                  fail
                  ( \(i, rest) -> do
                      unless (LazyText.null rest) . liftIO $
                        assertFailure ("The rest of output after parsed Int must be empty, but it was: " <> LazyText.unpack rest)
                      pure i
                  )
                  . signed decimal
                  . LazyText.strip
                  . LazyTextEncoding.decodeUtf8
                  <=< interpretWatModule [] . compile
                $ t
            let outInterpreted = interpret t
            pure $ outInterpreted == outCompiled
        ]
