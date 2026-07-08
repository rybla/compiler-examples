{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utilities.Unsafe where

todo :: String -> a
todo msg = error $ "[" <> "TODO" <> "] " <> msg
