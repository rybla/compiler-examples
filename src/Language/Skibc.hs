-- | The SKIBC combinator calculus
module Language.Skibc where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Language.Godel (Godel (encodeGodel))
import Language.Wasm.Wat (CompileWat (compileWat))
import Language.Wasm.Wat qualified as Wat
import Test.QuickCheck (Arbitrary, oneof, sized)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Utilities (halve)

--------------------------------
-- Syntax
--------------------------------

data Tm
  = S
  | K
  | I
  | B
  | C
  | App1 Tm Tm
  deriving (Generic, Eq, Show, Ord)

pattern App2 :: Tm -> Tm -> Tm -> Tm
pattern App2 f x y = (f `App1` x) `App1` y

pattern App3 :: Tm -> Tm -> Tm -> Tm -> Tm
pattern App3 f x y z = App2 f x y `App1` z

instance Godel Tm where
  encodeGodel S = 1
  encodeGodel K = 2
  encodeGodel I = 3
  encodeGodel B = 4
  encodeGodel C = 5
  encodeGodel (App1 x y) = encodeGodel (6 :: Integer, (x, y))

instance Arbitrary Tm where
  arbitrary = sized genTm
    where
      genTm 0 = oneof . fmap pure $ leaves
      genTm n =
        oneof
          [ oneof . fmap pure $ leaves,
            App1 <$> genTm (halve n) <*> genTm (halve n)
          ]

      leaves = [S, K, I, B, C]

  shrink (App1 f a) =
    concat
      [ [f, a],
        [App1 f' a | f' <- shrink f],
        [App1 f a' | a' <- shrink a]
      ]
  shrink _ = []

--------------------------------
-- Interpretation
--------------------------------

evaluate :: Tm -> Tm
evaluate = evaluateLimit 1000

evaluateLimit :: Int -> Tm -> Tm
evaluateLimit 0 t = t
evaluateLimit n t = case deepStep t of
  Nothing -> t
  Just t' -> evaluateLimit (n - 1) t'

deepStep :: Tm -> Maybe Tm
deepStep t@(App1 f a) =
  step t
    <|> ((f `App1`) <$> deepStep a)
    <|> (deepStep f <&> (`App1` a))
deepStep _ = Nothing

step :: Tm -> Maybe Tm
step (App3 S x y z) = Just $ App2 x y z
step (App2 K x _) = Just x
step (App1 I x) = Just x
step (App3 B x y z) = Just $ App2 x y z
step (App3 C x y z) = Just $ App2 x z y
step _ = Nothing

--------------------------------
-- Compilation
--------------------------------

instance CompileWat () Tm where
  compileWat () tm =
    Wat.Module
      Nothing
      [ Wat.Type_Decl . Wat.Type . Wat.RecType $
          [ Wat.TypeDef
              (Just . Wat.Identifier $ "tm")
              ( Wat.SubType
                  Nothing
                  []
                  ( Wat.Struct_CompType
                      [ Wat.Field Nothing (Wat.FieldType False (Wat.ValType_StorageType i32ValType)),
                        Wat.Field Nothing (Wat.FieldType False (Wat.ValType_StorageType tmValType)),
                        Wat.Field Nothing (Wat.FieldType False (Wat.ValType_StorageType tmValType))
                      ]
                  )
              ),
            Wat.TypeDef
              (Just . Wat.Identifier $ "bigint")
              ( Wat.SubType
                  Nothing
                  []
                  ( Wat.Struct_CompType
                      (replicate 10 (Wat.Field Nothing (Wat.FieldType False (Wat.ValType_StorageType i64ValType))))
                  )
              ),
            makeFuncType "main_type" [] [i64ValType],
            makeFuncType "pair_type" [i64ValType, i64ValType] [i64ValType],
            makeFuncType "bigint_from_i64_type" [i64ValType] [bigintValType],
            makeFuncType "bigint_to_i64_type" [bigintValType] [i64ValType],
            makeFuncType "bigint_add_type" [bigintValType, bigintValType] [bigintValType],
            makeFuncType "bigint_mul_type" [bigintValType, bigintValType] [bigintValType],
            makeFuncType "bigint_div2_type" [bigintValType] [bigintValType],
            makeFuncType "bigint_pair_type" [bigintValType, bigintValType] [bigintValType],
            makeFuncType "godel_type" [tmValType] [bigintValType],
            makeFuncType "step_type" [tmValType] [tmValType],
            makeFuncType "deepStep_type" [tmValType] [tmValType],
            makeFuncType "evaluate_type" [tmValType] [tmValType]
          ],
        Wat.Func_Decl $ makeFunc "pair" "pair_type" [i64ValType] pairBody,
        Wat.Func_Decl $ makeFunc "bigint_from_i64" "bigint_from_i64_type" [] bigintFromI64Body,
        Wat.Func_Decl $ makeFunc "bigint_to_i64" "bigint_to_i64_type" [] bigintToI64Body,
        Wat.Func_Decl $ makeFunc "bigint_add" "bigint_add_type" [i64ValType, i64ValType] bigintAddBody,
        Wat.Func_Decl $ makeFunc "bigint_div2" "bigint_div2_type" [i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType] bigintDiv2Body,
        Wat.Func_Decl $ makeFunc "bigint_mul" "bigint_mul_type" [i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType, i64ValType] bigintMulBody,
        Wat.Func_Decl $ makeFunc "bigint_pair" "bigint_pair_type" [bigintValType, bigintValType] bigintPairBody,
        Wat.Func_Decl $ makeFunc "godel" "godel_type" [i32ValType, bigintValType, bigintValType, bigintValType, bigintValType] godelBody,
        Wat.Func_Decl $ makeFunc "step" "step_type" [tmValType, tmValType, tmValType, tmValType, tmValType, tmValType, tmValType] stepBody,
        Wat.Func_Decl $ makeFunc "deepStep" "deepStep_type" [tmValType, tmValType, tmValType] deepStepBody,
        Wat.Func_Decl $ makeFunc "evaluate" "evaluate_type" [tmValType, i32ValType] evaluateBody,
        Wat.Func_Decl $
          Wat.Func
            (Just . Wat.Identifier $ "main")
            (Just . Wat.TypeUse . Wat.TypeIdx . Wat.Identifier_Idx . Wat.Identifier $ "main_type")
            []
            (Wat.Expr (compileTm tm <> [Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "evaluate")))), Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "godel")))), Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_to_i64"))))])),
        Wat.Export_Decl $
          Wat.Export
            (Wat.Name "main")
            (Wat.Func_ExternIdx . Wat.FuncIdx . Wat.Identifier_Idx . Wat.Identifier $ "main")
      ]

tmTypeIdx :: Wat.TypeIdx
tmTypeIdx = Wat.TypeIdx . Wat.Identifier_Idx . Wat.Identifier $ "tm"

tmRefType :: Wat.RefType
tmRefType = Wat.RefType (Just Wat.Null) (Wat.TypeIdx_HeapType tmTypeIdx)

tmValType :: Wat.ValType
tmValType = Wat.RefType_ValType tmRefType

bigintTypeIdx :: Wat.TypeIdx
bigintTypeIdx = Wat.TypeIdx . Wat.Identifier_Idx . Wat.Identifier $ "bigint"

bigintRefType :: Wat.RefType
bigintRefType = Wat.RefType (Just Wat.Null) (Wat.TypeIdx_HeapType bigintTypeIdx)

bigintValType :: Wat.ValType
bigintValType = Wat.RefType_ValType bigintRefType

i32ValType :: Wat.ValType
i32ValType = Wat.NumType_ValType Wat.I32_NumType

i64ValType :: Wat.ValType
i64ValType = Wat.NumType_ValType Wat.I64_NumType

makeFuncType :: String -> [Wat.ValType] -> [Wat.ValType] -> Wat.TypeDef
makeFuncType id' params results =
  Wat.TypeDef
    (Just . Wat.Identifier . Text.pack $ id')
    ( Wat.SubType
        Nothing
        []
        ( Wat.Func_CompType
            (fmap (Wat.Param Nothing) params)
            (fmap Wat.Result results)
        )
    )

makeFunc :: String -> String -> [Wat.ValType] -> [Wat.Instr] -> Wat.Func
makeFunc name typeName locals instrs =
  Wat.Func
    (Just . Wat.Identifier . Text.pack $ name)
    (Just . Wat.TypeUse . Wat.TypeIdx . Wat.Identifier_Idx . Wat.Identifier . Text.pack $ typeName)
    (fmap (Wat.Local Nothing) locals)
    (Wat.Expr instrs)

pairBody :: [Wat.Instr]
pairBody =
  [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
    Wat.Plain_Instr Wat.I64Add_PlainInstr,
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
    Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
    Wat.Plain_Instr Wat.I64And_PlainInstr,
    Wat.Plain_Instr Wat.I64Eqz_PlainInstr,
    Wat.Block_Instr $
      Wat.If_BlockInstr
        Nothing
        (Wat.Result_BlockType (Just (Wat.Result i64ValType)))
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 2),
          Wat.Plain_Instr Wat.I64DivU_PlainInstr,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
          Wat.Plain_Instr Wat.I64Add_PlainInstr,
          Wat.Plain_Instr Wat.I64Mul_PlainInstr
        ]
        Nothing
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
          Wat.Plain_Instr Wat.I64Add_PlainInstr,
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 2),
          Wat.Plain_Instr Wat.I64DivU_PlainInstr,
          Wat.Plain_Instr Wat.I64Mul_PlainInstr
        ]
        Nothing,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
    Wat.Plain_Instr Wat.I64Add_PlainInstr
  ]

bigintFromI64Body :: [Wat.Instr]
bigintFromI64Body =
  [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.I64Const_PlainInstr 4294967295),
    Wat.Plain_Instr Wat.I64And_PlainInstr,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.I64Const_PlainInstr 32),
    Wat.Plain_Instr Wat.I64ShrU_PlainInstr
  ]
    <> ( replicate 8 (Wat.Plain_Instr (Wat.I64Const_PlainInstr 0))
           <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr bigintTypeIdx)]
       )

bigintToI64Body :: [Wat.Instr]
bigintToI64Body =
  [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
    Wat.Plain_Instr (Wat.I64Const_PlainInstr 32),
    Wat.Plain_Instr Wat.I64Shl_PlainInstr,
    Wat.Plain_Instr Wat.I64Add_PlainInstr
  ]

bigintAddBody :: [Wat.Instr]
bigintAddBody =
  [ Wat.Plain_Instr (Wat.I64Const_PlainInstr 0),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2)))
  ]
    <> ( concat
           [ [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
               Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx i))),
               Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
               Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx i))),
               Wat.Plain_Instr Wat.I64Add_PlainInstr,
               Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
               Wat.Plain_Instr Wat.I64Add_PlainInstr,
               Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
               Wat.Plain_Instr (Wat.I64Const_PlainInstr 4294967295),
               Wat.Plain_Instr Wat.I64And_PlainInstr,
               Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
               Wat.Plain_Instr (Wat.I64Const_PlainInstr 32),
               Wat.Plain_Instr Wat.I64ShrU_PlainInstr,
               Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2)))
             ]
           | i <- [0 .. 9]
           ]
           <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr bigintTypeIdx)]
       )

bigintDiv2Body :: [Wat.Instr]
bigintDiv2Body =
  [ Wat.Plain_Instr (Wat.I64Const_PlainInstr 0),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1)))
  ]
    <> ( concat
           [ [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
               Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx i))),
               Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
               Wat.Plain_Instr (Wat.I64Const_PlainInstr 32),
               Wat.Plain_Instr Wat.I64Shl_PlainInstr,
               Wat.Plain_Instr Wat.I64Add_PlainInstr,
               Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
               Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
               Wat.Plain_Instr Wat.I64ShrU_PlainInstr,
               Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx (3 + i)))),
               Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
               Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
               Wat.Plain_Instr Wat.I64And_PlainInstr,
               Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1)))
             ]
           | i <- reverse [0 .. 9]
           ]
           <> [Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx r))) | r <- [3 .. 12]]
           <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr bigintTypeIdx)]
       )

bigintMulBody :: [Wat.Instr]
bigintMulBody =
  ( concat
      [ [ Wat.Plain_Instr (Wat.I64Const_PlainInstr 0),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx r)))
        ]
      | r <- [4 .. 13]
      ]
  )
    <> ( concat
           [ [ Wat.Plain_Instr (Wat.I64Const_PlainInstr 0),
               Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2)))
             ]
               <> concat
                 [ [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
                     Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx i))),
                     Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
                     Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx j))),
                     Wat.Plain_Instr Wat.I64Mul_PlainInstr,
                     Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx (4 + idx)))),
                     Wat.Plain_Instr Wat.I64Add_PlainInstr,
                     Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                     Wat.Plain_Instr Wat.I64Add_PlainInstr,
                     Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                     Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                     Wat.Plain_Instr (Wat.I64Const_PlainInstr 4294967295),
                     Wat.Plain_Instr Wat.I64And_PlainInstr,
                     Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx (4 + idx)))),
                     Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                     Wat.Plain_Instr (Wat.I64Const_PlainInstr 32),
                     Wat.Plain_Instr Wat.I64ShrU_PlainInstr,
                     Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2)))
                   ]
                 | j <- [0 .. 9 - i],
                   let idx = i + j
                 ]
           | i <- [0 .. 9]
           ]
           <> [Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx r))) | r <- [4 .. 13]]
           <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr bigintTypeIdx)]
       )

bigintPairBody :: [Wat.Instr]
bigintPairBody =
  [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
    Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_add")))),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
    Wat.Plain_Instr (Wat.StructGet_PlainInstr bigintTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
    Wat.Plain_Instr Wat.I64And_PlainInstr,
    Wat.Plain_Instr Wat.I64Eqz_PlainInstr,
    Wat.Block_Instr $
      Wat.If_BlockInstr
        Nothing
        (Wat.Result_BlockType (Just (Wat.Result bigintValType)))
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_div2")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_from_i64")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_add")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_mul"))))
        ]
        Nothing
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.I64Const_PlainInstr 1),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_from_i64")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_add")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_div2")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_mul"))))
        ]
        Nothing,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
    Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_add"))))
  ]

godelBody :: [Wat.Instr]
godelBody =
  [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
    Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
    Wat.Block_Instr $
      Wat.If_BlockInstr
        Nothing
        (Wat.Result_BlockType (Just (Wat.Result bigintValType)))
        [ Wat.Plain_Instr (Wat.I64Const_PlainInstr 0),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_from_i64"))))
        ]
        Nothing
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
          Wat.Plain_Instr Wat.I32LtU_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType (Just (Wat.Result bigintValType)))
              [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
                Wat.Plain_Instr Wat.I64ExtendI32U_PlainInstr,
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_from_i64"))))
              ]
              Nothing
              [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
                Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "godel")))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
                Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "godel")))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_pair")))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
                Wat.Plain_Instr (Wat.I64Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_from_i64")))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "bigint_pair"))))
              ]
              Nothing
        ]
        Nothing
  ]

stepBody :: [Wat.Instr]
stepBody =
  [ Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
    Wat.Block_Instr $
      Wat.Block_BlockInstr
        (Just (Wat.Identifier "done"))
        (Wat.Result_BlockType Nothing)
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 6))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 3),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 6))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 5))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 2),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 5))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 5))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 6))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 5))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 6))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 4))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 6))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 5))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing
        ]
        Nothing,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 7)))
  ]

deepStepBody :: [Wat.Instr]
deepStepBody =
  [ Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
    Wat.Block_Instr $
      Wat.Block_BlockInstr
        (Just (Wat.Identifier "done"))
        (Wat.Result_BlockType Nothing)
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "step")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.StructGet_PlainInstr tmTypeIdx (Wat.FieldIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          Wat.Block_Instr $
            Wat.If_BlockInstr
              Nothing
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 6),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done"))))
              ]
              Nothing
              []
              Nothing,
          Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3)))
        ]
        Nothing,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 3)))
  ]

evaluateBody :: [Wat.Instr]
evaluateBody =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 1000),
    Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
    Wat.Block_Instr $
      Wat.Block_BlockInstr
        (Just (Wat.Identifier "done"))
        (Wat.Result_BlockType Nothing)
        [ Wat.Block_Instr $
            Wat.Loop_BlockInstr
              (Just (Wat.Identifier "loop"))
              (Wat.Result_BlockType Nothing)
              [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
                Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                Wat.Plain_Instr Wat.I32Sub_PlainInstr,
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 2))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
                Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
                Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
                Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
                Wat.Plain_Instr (Wat.BrIf_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "done")))),
                Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
                Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
                Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "loop"))))
              ]
              Nothing
        ]
        Nothing,
    Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0)))
  ]

compileTm :: Tm -> [Wat.Instr]
compileTm S =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)
  ]
compileTm K =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 2),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)
  ]
compileTm I =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 3),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)
  ]
compileTm B =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)
  ]
compileTm C =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr (Wat.TypeIdx_HeapType tmTypeIdx)),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)
  ]
compileTm (App1 x y) =
  [Wat.Plain_Instr (Wat.I32Const_PlainInstr 6)]
    <> compileTm x
    <> compileTm y
    <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr tmTypeIdx)]
