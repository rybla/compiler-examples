module Language.Godel where

import Data.Int (Int16, Int32, Int64)
import Utilities (halve)

class Godel a where
  encodeGodel :: a -> Int64

instance Godel Int16 where
  encodeGodel = fromIntegral

instance Godel Int32 where
  encodeGodel = fromIntegral

instance Godel Int64 where
  encodeGodel = id

instance Godel Integer where
  encodeGodel = fromIntegral

instance Godel Int where
  encodeGodel = fromIntegral

instance (Godel a, Godel b) => Godel (a, b) where
  encodeGodel (a, b) = halve ((ga + gb) * (ga + gb + 1)) + gb
    where
      ga = encodeGodel a
      gb = encodeGodel b
