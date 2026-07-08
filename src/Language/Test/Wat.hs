{- HLINT ignore "Redundant $" -}

module Language.Test.Wat (spec) where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Wat
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), layoutPretty)
import Prettyprinter.Render.Text (renderLazy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Wat" $
    [ goldenVsString "x1" "asset/golden/Wat/x1.golden.txt" . return . encodeDocUtf8 . encodeWat $
        Module
          Nothing
          [ Export_Decl $
              Export
                (Name "return_default")
                (Func_ExternIdx $ FuncIdx $ Identifier_Idx $ Identifier $ "return_default"),
            Func_Decl $
              Func
                (Just . Identifier $ "return_default")
                Nothing
                [Local (Just . Identifier $ "x") (NumType_ValType I32_NumType)]
                ( Expr
                    [ Plain_Instr . LocalGet_PlainInstr . LocalIdx . Identifier_Idx . Identifier $ "x"
                    ]
                )
          ]
    ]

encodeDocUtf8 :: Doc ann -> LazyByteString
encodeDocUtf8 = encodeUtf8 . renderLazy . layoutPretty (LayoutOptions Unbounded)
