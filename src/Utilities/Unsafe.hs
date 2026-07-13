module Utilities.Unsafe where

todo :: String -> a
todo msg = error $ "[" <> "TODO" <> "]: " <> msg
