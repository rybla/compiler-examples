{- HLINT ignore "Redundant $" -}

module Language.Test.Lc where

-- import Control.Lens
-- import Control.Monad ((<=<))
-- import Control.Monad.Except (runExceptT)
-- import Control.Monad.State (evalStateT)
-- import Data.Text.Lazy qualified as LazyText
-- import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
-- import Language.Lc.Compilation
-- import Language.Lc.Interpretation
-- import Language.Lc.Syntax
-- import Language.Test.Common
-- import Prettyprinter (defaultLayoutOptions, layoutPretty)
-- import Prettyprinter.Render.Text (renderLazy)
-- import Test.Tasty (TestTree, testGroup)
-- import Test.Tasty.QuickCheck (idempotentIOProperty, testProperty)

-- --------------------------------

-- spec :: TestTree
-- spec =
--   testGroup "Lc" $
--     let dp = "asset/golden/Lc/"
--      in [ testGroup "examples" $
--             [ testWatModule "x1" dp [] . compile $
--                 App (Lam (Var 0)) (Lit "hello"),
--               testWatModule "x2" dp [] . compile $
--                 App (App (Lam (Lam (App (Var 1) (Var 0)))) (Lam (Var 0))) (Lit "hello")
--             ],
--           testProperty "compiler-correct" $ \(t :: Tm) -> idempotentIOProperty $ do
--             outCompiled :: Tm <-
--               either (fail . LazyText.unpack) pure <=< runExceptT
--                 $ pure
--                   . Lit
--                   . LazyText.toStrict
--                   . LazyText.strip
--                   . LazyTextEncoding.decodeUtf8
--                   <=< interpretWatModule [] . compile
--                 $ t
--             outInterpreted :: Tm <-
--               normalize t
--                 & flip evalStateT 100000
--                 & runExceptT
--                 >>= \case
--                   Left err -> fail . LazyText.unpack . renderLazy . layoutPretty defaultLayoutOptions $ err
--                   Right a -> pure a
--             pure $ outInterpreted == outCompiled
--         ]
