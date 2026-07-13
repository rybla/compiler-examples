{- HLINT ignore "Redundant $" -}

module Language.Test.Skibc where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Skibc
import Language.Test.Common
import Language.Wasm.Wat (compileWat)
import Test.QuickCheck (idempotentIOProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (testProperty)
import Text.Read (readMaybe)

--------------------------------

spec :: TestTree
spec =
  testGroup "Skibc" $
    let goldenPath = "asset/golden/Skibc/"
     in [ testGroup "examples" $
            [ testWatModule "x1" goldenPath [] . compileWat () $
                App3 S B I I
            ],
          testProperty "compiler-correctness" $ \t -> idempotentIOProperty $ do
            let outInterpreted = evaluate t
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
