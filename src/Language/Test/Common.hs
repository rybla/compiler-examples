{- HLINT ignore "Redundant $" -}

module Language.Test.Common where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Wasm.Wat
import Language.Wasm.Wat.Utilities (formatWat, toWasm)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Test.Tasty (DependencyType (AllSucceed), TestName, TestTree, after, testGroup)
import Test.Tasty.Golden (goldenVsString)

dirpathTmp :: FilePath
dirpathTmp = "tmp"

testWatModule :: TestName -> FilePath -> Module -> TestTree
testWatModule name dp m =
  testGroup name $
    [ goldenVsString "wat" (dp <> name <> ".golden.wat") $
        encodeWatModuleAsUtf8 m,
      after AllSucceed (name <> ".wat")
        . goldenVsString "wasm" (dp <> name <> ".golden.wasm")
        . (either (fail . ("Error:\n\n" <>) . LazyText.unpack) pure <=< runExceptT)
        $ toWasm m,
      after AllSucceed (name <> ".wasm")
        . goldenVsString "interp" (dp <> name <> ".out.golden.txt")
        . (either (fail . ("Error:\n\n" <>) . LazyText.unpack) pure <=< runExceptT)
        $ interpretWasmFile (dp <> name <> ".golden.wasm")
    ]

encodeWatModuleAsUtf8 :: Module -> IO LazyByteString
encodeWatModuleAsUtf8 =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . formatWat

interpretWasmFile :: FilePath -> ExceptT LazyText IO LazyByteString
interpretWasmFile fp = do
  (errorCode, out, err) <- liftIO $ readProcessWithExitCode "wasmtime" ["run", "-W", "gc=y", "--invoke", "main", fp] ""
  case errorCode of
    ExitFailure _ -> pure $ "Error\n\n" <> err
    ExitSuccess -> pure out

interpretWatModule :: Module -> ExceptT LazyText IO LazyByteString
interpretWatModule m = do
  t <- toWasm m
  (errorCode, out, err) <- liftIO $ readProcessWithExitCode "wasmtime" ["run", "-W", "gc=y", "--invoke", "main", "-"] t
  case errorCode of
    ExitFailure _ -> pure $ "Error\n\n" <> err
    ExitSuccess -> pure out
