{- HLINT ignore "Use camelCase" -}

-- | The SKIBC combinator calculus.
module Language.Skibc where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (MonadState (get), put)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Wasm.Wat (CompileWat (compileWat))
import Language.Wasm.Wat qualified as Wat
import Numeric.Natural (Natural, minusNaturalMaybe)
import Prettyprinter (Doc, Pretty (pretty), dquotes, hsep, (<+>))
import Test.QuickCheck (Arbitrary, oneof, sized)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Utilities (halve)

--------------------------------
-- Syntax
--------------------------------

data Term
  = S
  | K
  | I
  | B
  | C
  | App1 Term Term
  deriving (Generic, Eq, Show, Read, Ord)

instance Pretty Term where
  pretty S = "S"
  pretty K = "K"
  pretty I = "I"
  pretty B = "B"
  pretty C = "C"
  pretty (App1 f a) = hsep $ go f [pretty a]
    where
      go (App1 f' a') ps = go f' (pretty a' : ps)
      go a' ps = pretty a' : ps

pattern App2 :: Term -> Term -> Term -> Term
pattern App2 f x y = (f `App1` x) `App1` y

pattern App3 :: Term -> Term -> Term -> Term -> Term
pattern App3 f x y z = App2 f x y `App1` z

instance Arbitrary Term where
  arbitrary = sized genTerm
    where
      genTerm 0 = oneof . fmap pure $ leaves
      genTerm n =
        oneof
          [ oneof . fmap pure $ leaves,
            App1 <$> genTerm (halve n) <*> genTerm (halve n)
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

evaluate :: (MonadState Natural m, MonadError (Doc Void) m) => Term -> m Term
evaluate t = do
  gas <- get
  case minusNaturalMaybe gas 1 of
    Nothing -> throwError $ "Attempted to evaluate" <+> (dquotes . pretty) t <+> "when out of gas"
    Just gas' -> put gas'
  case deepStep t of
    Nothing -> pure t
    Just t' -> evaluate t'

deepStep :: Term -> Maybe Term
deepStep t@(App1 f a) =
  step t
    <|> ((f `App1`) <$> deepStep a)
    <|> (deepStep f <&> (`App1` a))
deepStep _ = Nothing

step :: Term -> Maybe Term
step (App3 S x y z) = Just $ App2 x z (App1 y z)
step (App2 K x _) = Just x
step (App1 I x) = Just x
step (App3 B x y z) = Just $ App1 x (App1 y z)
step (App3 C x y z) = Just $ App2 x z y
step _ = Nothing

--------------------------------
-- Compilation
--------------------------------

termTypeIdx :: Wat.TypeIdx
termTypeIdx = Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "term"))

termHeapType :: Wat.HeapType
termHeapType = Wat.TypeIdx_HeapType termTypeIdx

termRefType :: Wat.RefType
termRefType = Wat.RefType (Just Wat.Null) termHeapType

termType :: Wat.Type
termType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "term"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Struct_CompType
              [ Wat.Field (Just (Wat.Identifier "tag")) (Wat.FieldType False (Wat.ValType_StorageType (Wat.NumType_ValType Wat.I32_NumType))),
                Wat.Field (Just (Wat.Identifier "left")) (Wat.FieldType False (Wat.ValType_StorageType (Wat.RefType_ValType termRefType))),
                Wat.Field (Just (Wat.Identifier "right")) (Wat.FieldType False (Wat.ValType_StorageType (Wat.RefType_ValType termRefType)))
              ]
          )
      )

fdWriteType :: Wat.Type
fdWriteType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "fd_write_type"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Func_CompType
              [ Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType),
                Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType),
                Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType),
                Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType)
              ]
              [Wat.Result (Wat.NumType_ValType Wat.I32_NumType)]
          )
      )

mainType :: Wat.Type
mainType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "main_type"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Func_CompType
              []
              []
          )
      )

printStrType :: Wat.Type
printStrType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "print_str_type"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Func_CompType
              [ Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType),
                Wat.Param Nothing (Wat.NumType_ValType Wat.I32_NumType)
              ]
              []
          )
      )

stepType :: Wat.Type
stepType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "step_type"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Func_CompType
              [Wat.Param Nothing (Wat.RefType_ValType termRefType)]
              [Wat.Result (Wat.RefType_ValType termRefType)]
          )
      )

printType :: Wat.Type
printType =
  Wat.Type . Wat.RecType . List.singleton $
    Wat.TypeDef
      (Just (Wat.Identifier "print_type"))
      ( Wat.SubType
          Nothing
          []
          ( Wat.Func_CompType
              [Wat.Param Nothing (Wat.RefType_ValType termRefType)]
              []
          )
      )

fdWriteImport :: Wat.Import
fdWriteImport =
  Wat.Import
    (Wat.Name "wasi_snapshot_preview1")
    (Wat.Name "fd_write")
    ( Wat.Func_ExternalType
        (Just (Wat.Identifier "fd_write"))
        (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "fd_write_type"))))
    )

memory :: Wat.Mem
memory =
  Wat.Mem
    (Just (Wat.Identifier "memory"))
    (Wat.MemType Nothing (Wat.Limits 1 Nothing))

memoryExport :: Wat.Export
memoryExport =
  Wat.Export
    (Wat.Name "memory")
    (Wat.Memory_ExternIdx (Wat.MemIdx (Wat.Identifier_Idx (Wat.Identifier "memory"))))

mainExport :: Wat.Export
mainExport =
  Wat.Export
    (Wat.Name "main")
    (Wat.Func_ExternIdx (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "main"))))

dataSegments :: [Wat.DataSegment]
dataSegments =
  [ makeDataSegment dataSegmentOffset_S "S",
    makeDataSegment dataSegmentOffset_K "K",
    makeDataSegment dataSegmentOffset_I "I",
    makeDataSegment dataSegmentOffset_B "B",
    makeDataSegment dataSegmentOffset_C "C",
    makeDataSegment dataSegmentOffset_App1 "App1 ",
    makeDataSegment dataSegmentOffset_LParen "(",
    makeDataSegment dataSegmentOffset_Space " ",
    makeDataSegment dataSegmentOffset_RParen ")",
    makeDataSegment dataSegmentOffset_Newline "\n"
  ]
  where
    makeDataSegment :: Int32 -> String -> Wat.DataSegment
    makeDataSegment offset str =
      Wat.DataSegment
        Nothing
        ( Wat.Active_DataMode
            (Wat.MemIdx (Wat.Index_Idx 0))
            (Wat.Expr [Wat.Plain_Instr (Wat.I32Const_PlainInstr offset)])
        )
        (BS8.pack str)

dataSegmentOffset_S, dataSegmentOffset_K, dataSegmentOffset_I, dataSegmentOffset_B, dataSegmentOffset_C, dataSegmentOffset_App1, dataSegmentOffset_LParen, dataSegmentOffset_Space, dataSegmentOffset_RParen, dataSegmentOffset_Newline :: Int32
dataSegmentOffset_S = 100
dataSegmentOffset_K = 101
dataSegmentOffset_I = 102
dataSegmentOffset_B = 103
dataSegmentOffset_C = 104
dataSegmentOffset_App1 = 105
dataSegmentOffset_LParen = 111
dataSegmentOffset_Space = 114
dataSegmentOffset_RParen = 115
dataSegmentOffset_Newline = 116

if_ :: [Wat.Instr] -> [Wat.Instr] -> Wat.Instr
if_ thenB elseB =
  Wat.Block_Instr $
    Wat.If_BlockInstr
      Nothing
      (Wat.Result_BlockType Nothing)
      thenB
      Nothing
      elseB
      Nothing

loop_ :: Wat.Identifier -> [Wat.Instr] -> Wat.Instr
loop_ label body =
  Wat.Block_Instr $
    Wat.Loop_BlockInstr
      (Just label)
      (Wat.Result_BlockType Nothing)
      body
      Nothing

getTag :: Wat.Instr
getTag =
  Wat.Plain_Instr $
    Wat.StructGet_PlainInstr
      termTypeIdx
      (Wat.FieldIdx (Wat.Identifier_Idx (Wat.Identifier (Text.pack "tag"))))

getLeft :: Wat.Instr
getLeft =
  Wat.Plain_Instr $
    Wat.StructGet_PlainInstr
      termTypeIdx
      (Wat.FieldIdx (Wat.Identifier_Idx (Wat.Identifier (Text.pack "left"))))

getRight :: Wat.Instr
getRight =
  Wat.Plain_Instr $
    Wat.StructGet_PlainInstr
      termTypeIdx
      (Wat.FieldIdx (Wat.Identifier_Idx (Wat.Identifier (Text.pack "right"))))

stepLocals :: [Wat.Local]
stepLocals =
  [ Wat.Local (Just (Wat.Identifier "f")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "a")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "f_left")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "f_right")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "f_left_left")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "f_left_right")) (Wat.RefType_ValType termRefType)
  ]

deepStepLocals :: [Wat.Local]
deepStepLocals =
  [ Wat.Local (Just (Wat.Identifier "res")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "f")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "a")) (Wat.RefType_ValType termRefType)
  ]

evaluateLocals :: [Wat.Local]
evaluateLocals =
  [ Wat.Local (Just (Wat.Identifier "current")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "next")) (Wat.RefType_ValType termRefType)
  ]

printLocals :: [Wat.Local]
printLocals =
  [ Wat.Local (Just (Wat.Identifier "f")) (Wat.RefType_ValType termRefType),
    Wat.Local (Just (Wat.Identifier "a")) (Wat.RefType_ValType termRefType)
  ]

printStrFunc :: Wat.Func
printStrFunc =
  Wat.Func
    (Just (Wat.Identifier "print_str"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "print_str_type")))))
    []
    ( Wat.Expr
        [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 0),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.I32Store_PlainInstr (Wat.MemIdx (Wat.Index_Idx 0)) (Wat.MemArg Nothing Nothing)),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 1))),
          Wat.Plain_Instr (Wat.I32Store_PlainInstr (Wat.MemIdx (Wat.Index_Idx 0)) (Wat.MemArg Nothing Nothing)),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 0),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 20),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "fd_write")))),
          Wat.Plain_Instr Wat.Drop_PlainInstr
        ]
    )

stepFunc :: Wat.Func
stepFunc =
  Wat.Func
    (Just (Wat.Identifier "step"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "step_type")))))
    stepLocals
    ( Wat.Expr
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getLeft,
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getRight,
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
              getTag,
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 2),
              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
              if_
                [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                  Wat.Plain_Instr Wat.Return_PlainInstr
                ]
                []
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
              getTag,
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
              if_
                [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
                  getLeft,
                  Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
                  getRight,
                  Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_right")))),
                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                  Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
                  Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
                  if_
                    [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                      getTag,
                      Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                      Wat.Plain_Instr Wat.I32Eq_PlainInstr,
                      if_
                        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_right")))),
                          Wat.Plain_Instr Wat.Return_PlainInstr
                        ]
                        []
                    ]
                    [],
                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                  Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
                  Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
                  if_
                    [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                      getTag,
                      Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                      Wat.Plain_Instr Wat.I32Eq_PlainInstr,
                      if_
                        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                          getLeft,
                          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_left")))),
                          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left")))),
                          getRight,
                          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_right")))),
                          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_left")))),
                          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
                          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
                          if_
                            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_left")))),
                              getTag,
                              Wat.Plain_Instr (Wat.I32Const_PlainInstr 0),
                              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
                              if_
                                [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_right")))),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_right")))),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr Wat.Return_PlainInstr
                                ]
                                [],
                              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_left")))),
                              getTag,
                              Wat.Plain_Instr (Wat.I32Const_PlainInstr 3),
                              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
                              if_
                                [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_right")))),
                                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_right")))),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr Wat.Return_PlainInstr
                                ]
                                [],
                              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_left")))),
                              getTag,
                              Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
                              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
                              if_
                                [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_left_right")))),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f_right")))),
                                  Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
                                  Wat.Plain_Instr Wat.Return_PlainInstr
                                ]
                                []
                            ]
                            []
                        ]
                        []
                    ]
                    []
                ]
                []
            ]
            [],
          Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType)
        ]
    )

deepStepFunc :: Wat.Func
deepStepFunc =
  Wat.Func
    (Just (Wat.Identifier "deepStep"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "step_type")))))
    deepStepLocals
    ( Wat.Expr
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
          Wat.Plain_Instr Wat.I32Ne_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getLeft,
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getRight,
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "step")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
              Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
          Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
          Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "res")))),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
              Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType)
        ]
    )

evaluateFunc :: Wat.Func
evaluateFunc =
  Wat.Func
    (Just (Wat.Identifier "evaluate"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "step_type")))))
    evaluateLocals
    ( Wat.Expr
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "current")))),
          loop_
            (Wat.Identifier "eval_loop")
            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "current")))),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "deepStep")))),
              Wat.Plain_Instr (Wat.LocalTee_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "next")))),
              Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
              Wat.Plain_Instr Wat.I32Eqz_PlainInstr,
              if_
                [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "next")))),
                  Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "current")))),
                  Wat.Plain_Instr (Wat.Br_PlainInstr (Wat.LabelIdx (Wat.Identifier_Idx (Wat.Identifier "eval_loop"))))
                ]
                []
            ],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "current"))))
        ]
    )

printFunc :: Wat.Func
printFunc =
  Wat.Func
    (Just (Wat.Identifier "print"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "print_type")))))
    printLocals
    ( Wat.Expr
        [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          Wat.Plain_Instr Wat.RefIsNull_PlainInstr,
          if_
            [Wat.Plain_Instr Wat.Return_PlainInstr]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 0),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_S),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_K),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 2),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_I),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 3),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_B),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_C),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr Wat.Return_PlainInstr
            ]
            [],
          Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
          getTag,
          Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
          Wat.Plain_Instr Wat.I32Eq_PlainInstr,
          if_
            [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
              getLeft,
              Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Index_Idx 0))),
              getRight,
              Wat.Plain_Instr (Wat.LocalSet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_App1),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
              getTag,
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
              if_
                [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_LParen),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print")))),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_RParen),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str"))))
                ]
                [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "f")))),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print"))))
                ],
              Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_Space),
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
              Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
              Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
              getTag,
              Wat.Plain_Instr (Wat.I32Const_PlainInstr 5),
              Wat.Plain_Instr Wat.I32Eq_PlainInstr,
              if_
                [ Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_LParen),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str")))),
                  Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print")))),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_RParen),
                  Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str"))))
                ]
                [ Wat.Plain_Instr (Wat.LocalGet_PlainInstr (Wat.LocalIdx (Wat.Identifier_Idx (Wat.Identifier "a")))),
                  Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print"))))
                ]
            ]
            []
        ]
    )

compileTerm :: Term -> [Wat.Instr]
compileTerm S =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 0),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)
  ]
compileTerm K =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)
  ]
compileTerm I =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 2),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)
  ]
compileTerm B =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 3),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)
  ]
compileTerm C =
  [ Wat.Plain_Instr (Wat.I32Const_PlainInstr 4),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.RefNull_PlainInstr termHeapType),
    Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)
  ]
compileTerm (App1 f a) =
  [Wat.Plain_Instr (Wat.I32Const_PlainInstr 5)]
    <> compileTerm f
    <> compileTerm a
    <> [Wat.Plain_Instr (Wat.StructNew_PlainInstr termTypeIdx)]

mainFunc :: Term -> Wat.Func
mainFunc t =
  Wat.Func
    (Just (Wat.Identifier "main"))
    (Just (Wat.TypeUse (Wat.TypeIdx (Wat.Identifier_Idx (Wat.Identifier "main_type")))))
    []
    ( Wat.Expr
        ( compileTerm t
            <> [ Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "evaluate")))),
                 Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print")))),
                 Wat.Plain_Instr (Wat.I32Const_PlainInstr dataSegmentOffset_Newline),
                 Wat.Plain_Instr (Wat.I32Const_PlainInstr 1),
                 Wat.Plain_Instr (Wat.Call_PlainInstr (Wat.FuncIdx (Wat.Identifier_Idx (Wat.Identifier "print_str"))))
               ]
        )
    )

makeType :: Wat.TypeDef -> Wat.Decl
makeType typeDef = Wat.Type_Decl (Wat.Type (Wat.RecType [typeDef]))

instance CompileWat () Term where
  compileWat () t =
    Wat.Module (Just (Wat.Identifier "skibc_module")) $
      (Wat.Data_Decl <$> dataSegments)
        <> [ Wat.Type_Decl termType,
             Wat.Type_Decl fdWriteType,
             Wat.Type_Decl mainType,
             Wat.Type_Decl printStrType,
             Wat.Type_Decl stepType,
             Wat.Type_Decl printType,
             Wat.Import_Decl fdWriteImport,
             Wat.Mem_Decl memory,
             Wat.Export_Decl memoryExport,
             Wat.Func_Decl printStrFunc,
             Wat.Func_Decl stepFunc,
             Wat.Func_Decl deepStepFunc,
             Wat.Func_Decl evaluateFunc,
             Wat.Func_Decl printFunc,
             Wat.Func_Decl (mainFunc t),
             Wat.Export_Decl mainExport
           ]
