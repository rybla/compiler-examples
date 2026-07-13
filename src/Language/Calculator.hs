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

data Term
  = Literal Int64
  | Operation Operator Term Term
  deriving (Generic, Eq, Show, Ord)

data Operator
  = Plus
  | Times
  deriving (Generic, Eq, Show, Ord)

instance Godel Term where
  encodeGodel (Literal x) = encodeGodel (1 :: Int64, x)
  encodeGodel (Operation op a b) = encodeGodel (op, (a, b))

instance Godel Operator where
  encodeGodel Plus = 1
  encodeGodel Times = 2

instance Arbitrary Operator where
  arbitrary = elements [Plus, Times]

instance Arbitrary Term where
  arbitrary = sized genTerm
    where
      genTerm 0 = Literal <$> arbitrary
      genTerm n =
        oneof
          [ Literal <$> arbitrary,
            do
              a <- genTerm (halve n)
              b <- genTerm (halve n)
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

interpret :: Term -> Int64
interpret (Literal v) = v
interpret (Operation Plus t1 t2) = interpret t1 + interpret t2
interpret (Operation Times t1 t2) = interpret t1 * interpret t2

--------------------------------
-- Compilation
--------------------------------

instance Wat.CompileWat () Term where
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
            (Wat.Expr (compileTerm tm)),
        Wat.Export_Decl $
          Wat.Export
            (Wat.Name "main")
            (Wat.Func_ExternIdx . Wat.FuncIdx . Wat.Identifier_Idx . Wat.Identifier $ "main")
      ]

compileTerm :: Term -> [Wat.Instr]
compileTerm (Literal n) = [Wat.Plain_Instr (Wat.I64Const_PlainInstr n)]
compileTerm (Operation Plus t1 t2) = compileTerm t1 <> compileTerm t2 <> [Wat.Plain_Instr Wat.I64Add_PlainInstr]
compileTerm (Operation Times t1 t2) = compileTerm t1 <> compileTerm t2 <> [Wat.Plain_Instr Wat.I64Mul_PlainInstr]
