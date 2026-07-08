{- HLINT ignore "Redundant $" -}

module Language.Test.Wat (spec) where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Wat
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (Unbounded), layoutPretty, pretty)
import Prettyprinter.Render.Text (renderLazy)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Wat" $
    [ goldenVsString "x3" "asset/golden/Wat/x3.golden.wat" . return . encodeDocUtf8 . pretty . encodeWatSexp $
        Module
          Nothing
          [ Export_Decl $
              Export
                (Name "return_default")
                (Func_ExternIdx $ FuncIdx $ Identifier_Idx $ Identifier $ "return_default"),
            Type_Decl $
              Type . RecType $
                [ TypeDef
                    (Just . Identifier $ "return_default_type")
                    ( SubType
                        Nothing
                        []
                        ( Func_CompType
                            []
                            [Result . NumType_ValType $ I32_NumType]
                        )
                    )
                ],
            Func_Decl $
              Func
                (Just . Identifier $ "return_default")
                (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "return_default_type")
                [Local (Just . Identifier $ "x") (NumType_ValType I32_NumType)]
                ( Expr
                    [ Plain_Instr . LocalGet_PlainInstr . LocalIdx . Identifier_Idx . Identifier $ "x"
                    ]
                )
          ]
    ]

encodeDocUtf8 :: Doc ann -> LazyByteString
encodeDocUtf8 = encodeUtf8 . renderLazy . layoutPretty (LayoutOptions Unbounded)
