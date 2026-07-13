{- HLINT ignore "Redundant $" -}

module Language.Test.Calculator where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Data.Text.Lazy.Read (decimal, signed)
import Language.Calculator
import Language.Test.Common
import Language.Wasm.Wat (compileWat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (idempotentIOProperty, testProperty)

--------------------------------

spec :: TestTree
spec =
  testGroup "Calculator" $
    let goldenPath = "asset/golden/Calculator/"
     in [ testGroup "examples" $
            [ testWatModule "x1" goldenPath [] . compileWat () $
                Literal 42,
              testWatModule "x2" goldenPath [] . compileWat () $
                Operation Times (Operation Plus (Literal 3) (Literal 4)) (Literal 5)
            ],
          testProperty "compilation-correctness" $ \t -> idempotentIOProperty $ do
            let outInterpreted = interpret t
            outCompiled <-
              t
                & interpretWatModule [] . compileWat ()
                <&> signed decimal . LazyText.strip . LazyTextEncoding.decodeUtf8
                >>= either
                  fail
                  ( \(i, rest) -> do
                      unless (LazyText.null rest) . liftIO $
                        assertFailure ("The rest of output after parsed Int must be empty, but it was: " <> LazyText.unpack rest)
                      pure i
                  )
                & runExceptT
                >>= either (fail . LazyText.unpack) pure
            pure $ outInterpreted == outCompiled
        ]
