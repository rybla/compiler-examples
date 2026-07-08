{- HLINT ignore "Redundant $" -}

module Language.Test.Wasm.Wat (spec) where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Wasm.Wat
import Language.Wasm.Wat.Utilities (formatWat)
import Prettyprinter (LayoutOptions (LayoutOptions), PageWidth (Unbounded), layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Wat" $
    [ goldenVsString "x1" "asset/golden/Wat/x1.golden.wat" . encodeWatSexpAsUtf8 . encodeWatSexp $
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

encodeWatSexpAsUtf8 :: Sexp -> IO LazyByteString
encodeWatSexpAsUtf8 =
  either
    (fail . ("Invalid WAT: " <>) . Text.unpack)
    (pure . encodeUtf8 . LazyText.fromStrict)
    <=< runExceptT
      . formatWat
      . renderStrict
      . layoutPretty (LayoutOptions Unbounded)
      . pretty
