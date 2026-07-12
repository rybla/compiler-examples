-- | The SKIBC combinator calculus
module Language.Skibc where

import Control.Applicative ((<|>))
import Control.Lens
import GHC.Generics (Generic)
import Language.Godel (Godel (encodeGodel))
import Language.Wasm.Wat (CompileWat (compileWat))
import Test.QuickCheck (Arbitrary, oneof, sized)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Utilities (halve)
import Utilities.Unsafe (todo)

--------------------------------
-- Syntax
--------------------------------

data Tm
  = S
  | K
  | I
  | B
  | C
  | App1 Tm Tm
  deriving (Generic, Eq, Show, Ord)

pattern App2 :: Tm -> Tm -> Tm -> Tm
pattern App2 f x y = (f `App1` x) `App1` y

pattern App3 :: Tm -> Tm -> Tm -> Tm -> Tm
pattern App3 f x y z = App2 f x y `App1` z

instance Godel Tm where
  encodeGodel S = 1
  encodeGodel K = 2
  encodeGodel I = 3
  encodeGodel B = 4
  encodeGodel C = 5
  encodeGodel (App1 x y) = encodeGodel (6 :: Integer, (x, y))

instance Arbitrary Tm where
  arbitrary = sized genTm
    where
      genTm 0 = oneof . fmap pure $ leaves
      genTm n =
        oneof
          [ oneof . fmap pure $ leaves,
            App1 <$> genTm (halve n) <*> genTm (halve n)
          ]

      leaves = [S, K, I, B, C]

  shrink (App1 f a) =
    concat
      [ [f, a],
        [App1 f' a | f' <- shrink f],
        [App1 f a' | a' <- shrink a]
      ]
  shrink t = pure t

--------------------------------
-- Interpretation
--------------------------------

evaluate :: Tm -> Tm
evaluate t = deepStep t & maybe t evaluate

deepStep :: Tm -> Maybe Tm
deepStep t@(App1 f a) =
  step t
    <|> ((f `App1`) <$> deepStep a)
    <|> (deepStep f <&> (`App1` a))
deepStep _ = Nothing

step :: Tm -> Maybe Tm
step (App3 S x y z) = Just $ App2 x y z
step (App2 K x _) = Just x
step (App1 I x) = Just x
step (App3 B x y z) = Just $ App2 x y z
step (App3 C x y z) = Just $ App2 x z y
step _ = Nothing

--------------------------------
-- Compilation
--------------------------------

instance CompileWat () Tm where
  compileWat = todo "compile the term to a WebAssembly module that has a main function that evaluates the term, then calculate's the evaluated term's Godel number, then returns the evaluated term's Godel number"
