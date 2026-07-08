{- HLINT ignore "Redundant $" -}

module Language.Test.Common where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Wasm.Wat
import Language.Wasm.Wat.Utilities (formatWat, toWasm)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Temp (withSystemTempFile)
import System.Process.Text.Lazy (readProcessWithExitCode)
import Test.Tasty (DependencyType (AllSucceed), TestName, TestTree, after, testGroup)
import Test.Tasty.Golden (goldenVsString)

testWatModule :: TestName -> FilePath -> Module -> TestTree
testWatModule name dp m =
  testGroup name $
    [ goldenVsString "wat" (dp <> name <> ".golden.wat") $ encodeWatModuleAsUtf8 m,
      after AllSucceed (name <> ".wat") $ goldenVsString "wasm" (dp <> name <> ".golden.wasm") $ encodeWatModuleAsWasm m,
      after AllSucceed (name <> ".wasm") $ goldenVsString "interp" (dp <> name <> ".out.golden.txt") $ interpretWasmFile (dp <> name <> ".golden.wasm")
    ]

encodeWatModuleAsUtf8 :: Module -> IO LazyByteString
encodeWatModuleAsUtf8 =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . formatWat

encodeWatModuleAsWasm :: Module -> IO LazyByteString
encodeWatModuleAsWasm =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . toWasm

interpretWasmFile :: FilePath -> IO LazyByteString
interpretWasmFile fp = do
  (errorCode, out, err) <- readProcessWithExitCode "wasmtime" ["run", "-W", "gc=y", "--invoke", "main", fp] ""
  case errorCode of
    ExitFailure _ -> pure . encodeUtf8 $ "Error\n\n" <> err
    ExitSuccess -> pure . encodeUtf8 $ out

interpretWatModule :: Module -> IO LazyByteString
interpretWatModule m = do
  withSystemTempFile "x.wasm" $ \fp h -> do
    LazyByteString.hPut h =<< encodeWatModuleAsWasm m
    interpretWasmFile fp
