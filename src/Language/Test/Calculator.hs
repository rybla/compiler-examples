{- HLINT ignore "Redundant $" -}

module Language.Test.Calculator where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Data.Text.Lazy.IO qualified as LazyTextIO
import Data.Text.Lazy.Read (decimal)
import Language.Calculator.Compilation
import Language.Calculator.Interpretation (interpret)
import Language.Calculator.Syntax
import Language.Test.Common
import Language.Wasm.Wat (EncodeWat (encodeWatSexp))
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (renderLazy)
import System.IO (stderr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool)
import Test.Tasty.QuickCheck (ioProperty, testProperty)
import Utilities (layoutUnbounded)

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
        outCompiled <-
          either (fail . LazyText.unpack) pure
            <=< runExceptT
            $ either fail (\(i, rest) -> liftIO (assertBool ("The rest of output after parsed Int must be empty, but it was: " <> LazyText.unpack rest) (LazyText.null rest)) $> i)
              . decimal
              . LazyText.strip
              <=< ( \txt -> do
                      liftIO . LazyTextIO.hPutStrLn stderr $ "Compiled module:\n\n" <> txt
                      pure txt
                  )
              <=< pure . LazyTextEncoding.decodeUtf8
              <=< interpretWatModule
              <=< ( \m -> do
                      liftIO . LazyTextIO.hPutStrLn stderr $ "Compiled module:\n\n" <> (renderLazy . layoutUnbounded . pretty . encodeWatSexp) m
                      pure m
                  )
              <=< pure . compile
            $ t
        let outInterpreted = interpret t
        pure $ outInterpreted == outCompiled
    ]
  where
    dp = "asset/golden/Calculator/"
