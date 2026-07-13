{- HLINT ignore "Redundant $" -}

module Language.Test.Skibc where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalState, evalStateT)
import Data.Either (fromRight)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Skibc
import Language.Test.Common
import Language.Wasm.Wat (compileWat)
import Numeric.Natural (Natural)
import Test.QuickCheck (discard, idempotentIOProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (testProperty)
import Text.Read (readMaybe)

--------------------------------

spec :: TestTree
spec =
  testGroup "Skibc" $
    let goldenPath = "asset/golden/Skibc/"
        maxEvaluationSteps = 100000 :: Natural
     in [ testGroup "examples" $
            [ testWatModule "x1" goldenPath [] . compileWat () $
                App3 S B I I
            ],
          testProperty "evaluation-idempotence" $ \t ->
            fromRight discard . flip evalState maxEvaluationSteps . runExceptT $ do
              t' <- evaluate t
              t'' <- evaluate t'
              pure $ t' == t'',
          testProperty "compilation-correctness" $ \t -> idempotentIOProperty $ do
            outInterpreted <-
              evaluate t
                & runExceptT
                & flip evalStateT maxEvaluationSteps
                >>= either (const discard) pure
            outCompiled <-
              t
                & interpretWatModule [] . compileWat ()
                <&> readMaybe . LazyText.unpack . LazyText.strip . LazyTextEncoding.decodeUtf8
                >>= maybe
                  (fail "Failed to read output term")
                  ( \(i, rest) -> do
                      unless (LazyText.null rest) . liftIO $
                        assertFailure ("The rest of output after parsed Int must be empty, but it was: " <> LazyText.unpack rest)
                      pure i
                  )
                & runExceptT
                >>= either (fail . LazyText.unpack) pure
            pure $ outCompiled == outInterpreted
        ]
