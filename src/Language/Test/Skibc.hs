{- HLINT ignore "Redundant $" -}

module Language.Test.Skibc where

import Control.Monad (unless, (<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Data.Text.Lazy.Read (Reader, decimal, signed)
import Language.Godel (Godel (encodeGodel))
import Language.Skibc
import Language.Test.Common
import Language.Wasm.Wat (compileWat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure)
import Test.Tasty.QuickCheck (discard, idempotentIOProperty, testProperty)
import Utilities

--------------------------------

spec :: TestTree
spec =
  testGroup "Skibc" $
    let dp = "asset/golden/Skibc/"
     in [ testGroup "examples" $
            [ testWatModule "x1" dp [] . compileWat () $
                App3 S B I I
            ],
          testProperty "compiler-correct" $ \t ->
            case safeCastIntegerToInt64 . encodeGodel . evaluate $ t of
              Nothing -> discard
              Just outInterpreted ->
                idempotentIOProperty $ do
                  outCompiled <-
                    either (fail . LazyText.unpack) pure <=< runExceptT
                      $ either
                        fail
                        ( \(i, rest) -> do
                            unless (LazyText.null rest) . liftIO $
                              assertFailure ("The rest of output after parsed Int64 must be empty, but it was: " <> LazyText.unpack rest)
                            pure i
                        )
                        . signed (decimal :: Reader Int64)
                        . LazyText.strip
                        . LazyTextEncoding.decodeUtf8
                        <=< interpretWatModule [] . compileWat ()
                      $ t
                  pure $ outInterpreted == outCompiled
        ]
