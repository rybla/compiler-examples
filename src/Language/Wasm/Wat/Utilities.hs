module Language.Wasm.Wat.Utilities where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text.Lazy (LazyText)
import Language.Wasm.Wat (EncodeWatSexp (encodeWatSexp), Module)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (renderLazy)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Text.Lazy (readProcessWithExitCode)
import Utilities (layoutUnbounded)

formatWat :: Module -> ExceptT LazyText IO LazyText
formatWat m = do
  let t = renderLazy . layoutUnbounded . pretty . encodeWatSexp $ m
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode "wasm-tools" ["parse", "--wat", "-"] t
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwError err
  pure out

toWasm :: Module -> ExceptT LazyText IO LazyText
toWasm m = do
  let t = renderLazy . layoutUnbounded . pretty . encodeWatSexp $ m
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode "wasm-tools" ["parse", "-"] t
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwError err
  pure out
