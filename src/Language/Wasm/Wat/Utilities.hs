module Language.Wasm.Wat.Utilities where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Wasm.Wat (EncodeWat (encodeWat), Module)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (renderLazy)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.ByteString.Lazy qualified
import System.Process.Text.Lazy qualified
import Utilities (layoutUnbounded)

--------------------------------

formatWat :: Module -> ExceptT LazyText IO LazyText
formatWat m = do
  let t = renderLazy . layoutUnbounded . pretty . encodeWat $ m
  (exitCode, out, err) <- liftIO $ System.Process.Text.Lazy.readProcessWithExitCode "wasm-tools" ["parse", "--wat", "-"] t
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwError err
  pure out

toWasm :: Module -> ExceptT LazyText IO LazyByteString
toWasm m = do
  let t = LazyTextEncoding.encodeUtf8 . renderLazy . layoutUnbounded . pretty . encodeWat $ m
  (exitCode, out, err) <- liftIO $ System.Process.ByteString.Lazy.readProcessWithExitCode "wasm-tools" ["parse", "-"] t
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwError . LazyTextEncoding.decodeUtf8 $ err
  pure out
