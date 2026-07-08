module Language.Calculator.Compilation where

import Language.Calculator.Syntax
import Language.Wasm.Wat

--------------------------------

compile :: Tm -> Module
compile tm =
  Module
    Nothing
    [ Type_Decl . Type . RecType $
        [ TypeDef
            (Just . Identifier $ "main_type")
            ( SubType
                Nothing
                []
                ( Func_CompType
                    []
                    [Result . NumType_ValType $ I64_NumType]
                )
            )
        ],
      Func_Decl $
        Func
          (Just . Identifier $ "main")
          (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "main_type")
          []
          (Expr (compileTm tm)),
      Export_Decl $
        Export
          (Name "main")
          (Func_ExternIdx . FuncIdx . Identifier_Idx . Identifier $ "main")
    ]

compileTm :: Tm -> [Instr]
compileTm (Literal n) = [Plain_Instr (I64Const_PlainInstr n)]
compileTm (Operation Plus t1 t2) = compileTm t1 <> compileTm t2 <> [Plain_Instr I64Add_PlainInstr]
compileTm (Operation Times t1 t2) = compileTm t1 <> compileTm t2 <> [Plain_Instr I64Mul_PlainInstr]
