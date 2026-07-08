module Language.Wasm.Wat.Utilities where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)

formatWat :: Text -> ExceptT Text IO Text
formatWat t = do
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode "wasm-tools" ["parse", "--wat", "-"] (Text.unpack t)
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwError . Text.pack $ err
  pure . Text.pack $ out

-- toWasm :: Text -> IO (Either )