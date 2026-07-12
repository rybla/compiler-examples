module Language.Lc.Compilation where

import Control.Monad.Except (MonadError)
import Data.Void (Void)
import Language.Lc.Syntax
import Language.Wasm.Wat
import Numeric.Natural (Natural)
import Prettyprinter (Doc)
import Utilities.Unsafe

--------------------------------

compileTm :: (MonadError (Doc Void) m) => Tm -> m Module
compileTm tm = do
  decls :: [Decl] <- todo "module declarations"
  arity_main :: Natural <- todo "compute arity of term"
  instrs_main :: [Instr] <- todo "compile term to sequence of instructions"
  pure $
    Module
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
