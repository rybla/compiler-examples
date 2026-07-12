module Language.Godel where

import Data.Int (Int16, Int32, Int64)
import Utilities (halve)

-- | A type `a` has a Gödel numbering if each term of `a` can be injectively
-- mapped to an `Integer`.
--
-- [Injectivity]
--
--     @forall x y. 'encodeGodel' x == 'encodeGodel' y ==> x == y@
class Godel a where
  encodeGodel :: a -> Integer

instance Godel Int16 where
  encodeGodel = fromIntegral

instance Godel Int32 where
  encodeGodel = fromIntegral

instance Godel Int64 where
  encodeGodel = fromIntegral

instance Godel Integer where
  encodeGodel = id

instance Godel Int where
  encodeGodel = fromIntegral

instance (Godel a, Godel b) => Godel (a, b) where
  encodeGodel (a, b) = halve ((ga + gb) * (ga + gb + 1)) + gb
    where
      ga = encodeGodel a
      gb = encodeGodel b
