module Language.Lc.Compilation where

import Language.Lc.Syntax
import Language.Wasm.Wat
import Numeric.Natural (Natural)
import Utilities.Unsafe

--------------------------------

compile :: Tm -> Module
compile _tm =
  let decls :: [Decl]
      decls = todo "any other module declarations"

      arity_main :: Natural
      arity_main = todo "compute arity of term"

      instrs_main :: [Instr]
      instrs_main = todo "compile term to sequence of instructions"
   in Module
        Nothing
        ( decls
            <> [ Type_Decl . Type . RecType $
                   [ TypeDef
                       (Just . Identifier $ "main_type")
                       ( SubType
                           Nothing
                           []
                           ( Func_CompType
                               ( replicate (fromIntegral arity_main) . Param Nothing . NumType_ValType $
                                   I64_NumType
                               )
                               [Result . NumType_ValType $ I64_NumType]
                           )
                       )
                   ],
                 Func_Decl $
                   Func
                     (Just . Identifier $ "main")
                     (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "main_type")
                     []
                     (Expr instrs_main),
                 Export_Decl $
                   Export
                     (Name "main")
                     (Func_ExternIdx . FuncIdx . Identifier_Idx . Identifier $ "main")
               ]
        )
