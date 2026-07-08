module Lib where

import Data.Text.Lazy qualified as TL

greet :: TL.Text -> TL.Text
greet name = "Greetings, " <> TL.strip name <> "!"
