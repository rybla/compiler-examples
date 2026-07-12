module Language.Calculator where

import Data.Int (Int64)
import GHC.Generics (Generic)
import Language.Godel (Godel (encodeGodel))
import Language.Wasm.Wat qualified as Wat
import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), elements, oneof, sized)
import Utilities (halve)

--------------------------------
-- Syntax
--------------------------------

data Tm
  = Literal Int64
  | Operation Operator Tm Tm
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)

instance Godel Tm where
  encodeGodel (Literal x) = encodeGodel (1 :: Int64, x)
  encodeGodel (Operation op a b) = encodeGodel (op, (a, b))

instance Godel Operator where
  encodeGodel Plus = 1
  encodeGodel Times = 2

instance Arbitrary Operator where
  arbitrary = elements [Plus, Times]

instance Arbitrary Tm where
  arbitrary = sized genTm
    where
      genTm 0 = Literal <$> arbitrary
      genTm n =
        oneof
          [ Literal <$> arbitrary,
            do
              a <- genTm (halve n)
              b <- genTm (halve n)
              (\op -> Operation op a b) <$> arbitrary
          ]

  shrink (Literal n) = [Literal n' | n' <- shrink n]
  shrink (Operation op t1 t2) =
    concat
      [ [t1, t2],
        [Operation op' t1 t2 | op' <- shrink op],
        [Operation op t1' t2 | t1' <- shrink t1],
        [Operation op t1 t2' | t2' <- shrink t2]
      ]

--------------------------------
-- Interpretation
--------------------------------

interpret :: Tm -> Int64
interpret (Literal v) = v
interpret (Operation Plus t1 t2) = interpret t1 + interpret t2
interpret (Operation Times t1 t2) = interpret t1 * interpret t2

--------------------------------
-- Compilation
--------------------------------

instance Wat.CompileWat () Tm where
  compileWat () tm =
    Wat.Module
      Nothing
      [ Wat.Type_Decl . Wat.Type . Wat.RecType $
          [ Wat.TypeDef
              (Just . Wat.Identifier $ "main_type")
              ( Wat.SubType
                  Nothing
                  []
                  ( Wat.Func_CompType
                      []
                      [Wat.Result . Wat.NumType_ValType $ Wat.I64_NumType]
                  )
              )
          ],
        Wat.Func_Decl $
          Wat.Func
            (Just . Wat.Identifier $ "main")
            (Just . Wat.TypeUse . Wat.TypeIdx . Wat.Identifier_Idx . Wat.Identifier $ "main_type")
            []
            (Wat.Expr (compileTm tm)),
        Wat.Export_Decl $
          Wat.Export
            (Wat.Name "main")
            (Wat.Func_ExternIdx . Wat.FuncIdx . Wat.Identifier_Idx . Wat.Identifier $ "main")
      ]

compileTm :: Tm -> [Wat.Instr]
compileTm (Literal n) = [Wat.Plain_Instr (Wat.I64Const_PlainInstr n)]
compileTm (Operation Plus t1 t2) = compileTm t1 <> compileTm t2 <> [Wat.Plain_Instr Wat.I64Add_PlainInstr]
compileTm (Operation Times t1 t2) = compileTm t1 <> compileTm t2 <> [Wat.Plain_Instr Wat.I64Mul_PlainInstr]
