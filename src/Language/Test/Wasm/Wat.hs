{- HLINT ignore "Redundant $" -}

module Language.Test.Wasm.Wat (spec) where

import Language.Test.Common
import Language.Wasm.Wat
import Test.Tasty (TestTree, testGroup)

--------------------------------

spec :: TestTree
spec =
  testGroup "Wat" $
    [ testWat "x1-return_default" dp $
        Module
          Nothing
          [ Type_Decl $
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
                ),
            Export_Decl $
              Export
                (Name "main")
                (Func_ExternIdx $ FuncIdx $ Identifier_Idx $ Identifier $ "main"),
            Type_Decl $
              Type . RecType $
                [ TypeDef
                    (Just . Identifier $ "main_type")
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
                (Just . Identifier $ "main")
                (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "main_type")
                []
                ( Expr
                    [ Plain_Instr . Call_PlainInstr . FuncIdx . Identifier_Idx . Identifier $ "return_default"
                    ]
                )
          ]
    ]
  where
    dp = "asset/golden/Wat/"
