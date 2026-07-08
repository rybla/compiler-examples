{- HLINT ignore "Redundant $" -}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Test.Wat (spec) where

import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Wat
import Prettyprinter (LayoutOptions (LayoutOptions), PageWidth (Unbounded), layoutPretty)
import Prettyprinter.Render.Text (renderLazy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Wat" $
    -- [ goldenVsString "x1" "golden/Wat/x1.golden.txt" . return . encodeUtf8 . renderLazy . layoutPretty (LayoutOptions Unbounded) . encodeWat $
    --     Module
    --       Nothing
    --       [ Func_Decl $
    --           Func
    --             (Just . Identifier $ "return_default")
    --             undefined
    --             [Local (Just . Identifier $ "x") (NumType_ValType I32_NumType)]
    --             ( Expr
    --                 [ (_)
    --                 ]
    --             )
    --       ]
    -- ]
    []
