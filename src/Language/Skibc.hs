-- | The SKIBC combinator calculus.
module Language.Skibc where

import Control.Applicative ((<|>))
import Control.Lens
import GHC.Generics (Generic)
import Language.Wasm.Wat (CompileWat (compileWat))
import Test.QuickCheck (Arbitrary, oneof, sized)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Utilities (halve)
import Utilities.Unsafe

--------------------------------
-- Syntax
--------------------------------

data Term
  = S
  | K
  | I
  | B
  | C
  | App1 Term Term
  deriving (Generic, Eq, Show, Read, Ord)

pattern App2 :: Term -> Term -> Term -> Term
pattern App2 f x y = (f `App1` x) `App1` y

pattern App3 :: Term -> Term -> Term -> Term -> Term
pattern App3 f x y z = App2 f x y `App1` z

instance Arbitrary Term where
  arbitrary = sized genTerm
    where
      genTerm 0 = oneof . fmap pure $ leaves
      genTerm n =
        oneof
          [ oneof . fmap pure $ leaves,
            App1 <$> genTerm (halve n) <*> genTerm (halve n)
          ]

      leaves = [S, K, I, B, C]

  shrink (App1 f a) =
    concat
      [ [f, a],
        [App1 f' a | f' <- shrink f],
        [App1 f a' | a' <- shrink a]
      ]
  shrink _ = []

--------------------------------
-- Interpretation
--------------------------------

evaluate :: Term -> Term
evaluate = evaluateLimit 1000

evaluateLimit :: Int -> Term -> Term
evaluateLimit 0 t = t
evaluateLimit n t = case deepStep t of
  Nothing -> t
  Just t' -> evaluateLimit (n - 1) t'

deepStep :: Term -> Maybe Term
deepStep t@(App1 f a) =
  step t
    <|> ((f `App1`) <$> deepStep a)
    <|> (deepStep f <&> (`App1` a))
deepStep _ = Nothing

step :: Term -> Maybe Term
step (App3 S x y z) = Just $ App2 x y z
step (App2 K x _) = Just x
step (App1 I x) = Just x
step (App3 B x y z) = Just $ App2 x y z
step (App3 C x y z) = Just $ App2 x z y
step _ = Nothing

--------------------------------
-- Compilation
--------------------------------

instance CompileWat () Term where
  -- The module must encode data structures for SKIBC combinator terms.
  -- The module must define an "evaluate" function that evaluates a SKIBC combinator term.
  -- The module must define a "print" function that prints a SKIBC combinator term into a format that matches the format used by the `Show` and `Read` instances of `Term`.
  -- The module must export a "main" function that evaluates the original SKIBC combinator term `t` given to `compileWat` and prints the result.
  compileWat () _t = todo "Compile a SKIBC combinator term into a WebAssembly module"
