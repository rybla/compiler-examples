{- HLINT ignore "Redundant $" -}

module Language.Test.Common where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Wasm.Wat
import Language.Wasm.Wat.Utilities (formatWat, toWasm)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Text.Lazy (readProcessWithExitCode)
import Test.Tasty (DependencyType (AllSucceed), TestName, TestTree, after, testGroup)
import Test.Tasty.Golden (goldenVsString)

testWat :: TestName -> FilePath -> Module -> TestTree
testWat name dp m =
  testGroup name $
    [ goldenVsString "wat" (dp <> name <> ".golden.wat") $ encodeWatAsUtf8 m,
      after AllSucceed (name <> ".wat") $ goldenVsString "wasm" (dp <> name <> ".golden.wasm") $ encodeWatAsWasm m,
      after AllSucceed (name <> ".wasm") $ goldenVsString "interp" (dp <> name <> ".out.golden.txt") $ interpretWasm (dp <> name <> ".golden.wasm")
    ]

encodeWatAsUtf8 :: Module -> IO LazyByteString
encodeWatAsUtf8 =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . formatWat

encodeWatAsWasm :: Module -> IO LazyByteString
encodeWatAsWasm =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . toWasm

interpretWasm :: FilePath -> IO LazyByteString
interpretWasm fp = do
  (errorCode, out, err) <- readProcessWithExitCode "wasmtime" ["run", "-W", "gc=y", "--invoke", "main", fp] ""
  case errorCode of
    ExitFailure _ -> pure . encodeUtf8 $ "Error\n\n" <> err
    ExitSuccess -> pure . encodeUtf8 $ out
