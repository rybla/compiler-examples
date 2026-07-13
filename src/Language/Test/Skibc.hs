{- HLINT ignore "Redundant $" -}

module Language.Test.Skibc where

import Language.Skibc
import Language.Test.Common
import Language.Wasm.Wat (compileWat)
import Test.Tasty (TestTree, testGroup)

--------------------------------

spec :: TestTree
spec =
  testGroup "Skibc" $
    let dp = "asset/golden/Skibc/"
     in [ testGroup "examples" $
            [ testWatModule "x1" dp [] . compileWat () $
                App3 S B I I
            ] {-,
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
                      pure $ outInterpreted == outCompiled-}
        ]
