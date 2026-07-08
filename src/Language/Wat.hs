{- HLINT ignore "Use camelCase" -}

-- | Embedding of the WebAssembly Text Format (WAT) from the WebAssembly Specification Release 3.0 (2026-07-07).
module Language.Wat where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prettyprinter
import Prettyprinter.Util (reflow)

--------------------------------

class EncodeWat a where
  encodeWat :: a -> Doc ann

instance EncodeWat Text where
  encodeWat = reflow

--------------------------------

-- # 6 WebAssembly Text Format (WAT)

-- ## 6.2 Lexical Format

data Token = Token Text
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Token where
  encodeWat (Token t) = reflow t

-- ### 6.2.5 Annotation

data Annot = Annot AnnotId [Token]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Annot where
  encodeWat (Annot i ts) = parens $ "@" <> encodeWat i <+> (hcat . fmap ((" " <>) . encodeWat)) ts

data AnnotId = AnnotId Text
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat AnnotId where
  encodeWat (AnnotId t) = reflow t

-- ## 6.3 Values

-- ### 6.3.1 Integers

-- Use Haskell's `Integer`

instance EncodeWat Integer where
  encodeWat = pretty

-- ### 6.3.2 Floating Point

-- Use Haskell's `Float`

instance EncodeWat Float where
  encodeWat = pretty

-- ### 6.3.3 Strings

-- Use Haskell's `ByteString`

instance EncodeWat ByteString where
  encodeWat = dquotes . reflow . Text.pack . show

-- ### 6.3.4 Names

-- | Names are strings denoting a literal character sequence.
data Name = Name Text
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Name where
  encodeWat (Name t) = reflow t

-- ### 6.3.5 Identifiers

-- | Indices can be given in both numeric and symbolic form. Symbolic identifiers that stand in lieu of indices start with "$".
data Identifier = Identifier Text
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Identifier where
  encodeWat (Identifier t) = "$" <> reflow t

-- ## 6.4 Types

-- ### 6.4.1 Number Types

data NumType
  = I32_NumType
  | I64_NumType
  | F32_NumType
  | F64_NumType
  deriving (Generic, Eq, Show, Ord)

-- ### 6.4.2 Vector Types

instance EncodeWat NumType where
  encodeWat I32_NumType = "i32"
  encodeWat I64_NumType = "i64"
  encodeWat F32_NumType = "f32"
  encodeWat F64_NumType = "f64"

data VecType
  = V128_VecType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat VecType where
  encodeWat V128_VecType = "v128"

-- ### 6.4.3 Heap Types

data AbsHeapType
  = Any_AbsHeapType
  | Eq_AbsHeapType
  | I31_AbsHeapType
  | Struct_AbsHeapType
  | Array_AbsHeapType
  | None_AbsHeapType
  | Func_AbsHeapType
  | NoFunc_AbsHeapType
  | Exn_AbsHeapType
  | NoExn_AbsHeapType
  | Extern_AbsHeapType
  | NoExtern_AbsHeapType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat AbsHeapType where
  encodeWat Any_AbsHeapType = "any"
  encodeWat Eq_AbsHeapType = "eq"
  encodeWat I31_AbsHeapType = "i31"
  encodeWat Struct_AbsHeapType = "struct"
  encodeWat Array_AbsHeapType = "array"
  encodeWat None_AbsHeapType = "none"
  encodeWat Func_AbsHeapType = "func"
  encodeWat NoFunc_AbsHeapType = "nounc"
  encodeWat Exn_AbsHeapType = "exn"
  encodeWat NoExn_AbsHeapType = "noxn"
  encodeWat Extern_AbsHeapType = "extern"
  encodeWat NoExtern_AbsHeapType = "noxtern"

data HeapType
  = AbsHeapType_HeapType AbsHeapType
  | TypeIdx_HeapType TypeIdx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat HeapType where
  encodeWat (AbsHeapType_HeapType aht) = encodeWat aht
  encodeWat (TypeIdx_HeapType ti) = encodeWat ti

-- ### 6.4.4 Reference Types

data Null = Null
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Null where
  encodeWat Null = "null"

data RefType = RefType (Maybe Null) HeapType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat RefType where
  encodeWat (RefType nullM ht) = "(" <> "ref" <> maybe mempty ((" " <>) . encodeWat) nullM <> " " <> encodeWat ht <> ")"

anyRefType :: RefType
anyRefType = RefType (Just Null) (AbsHeapType_HeapType Any_AbsHeapType)

eqRefType :: RefType
eqRefType = RefType (Just Null) (AbsHeapType_HeapType Eq_AbsHeapType)

i31RefType :: RefType
i31RefType = RefType (Just Null) (AbsHeapType_HeapType I31_AbsHeapType)

structRefType :: RefType
structRefType = RefType (Just Null) (AbsHeapType_HeapType Struct_AbsHeapType)

arrayRefType :: RefType
arrayRefType = RefType (Just Null) (AbsHeapType_HeapType Array_AbsHeapType)

noneRefType :: RefType
noneRefType = RefType (Just Null) (AbsHeapType_HeapType None_AbsHeapType)

funcRefType :: RefType
funcRefType = RefType (Just Null) (AbsHeapType_HeapType Func_AbsHeapType)

noFuncRefType :: RefType
noFuncRefType = RefType (Just Null) (AbsHeapType_HeapType NoFunc_AbsHeapType)

exnRefType :: RefType
exnRefType = RefType (Just Null) (AbsHeapType_HeapType Exn_AbsHeapType)

noExnRefType :: RefType
noExnRefType = RefType (Just Null) (AbsHeapType_HeapType NoExn_AbsHeapType)

externRefType :: RefType
externRefType = RefType (Just Null) (AbsHeapType_HeapType Extern_AbsHeapType)

noExternRefType :: RefType
noExternRefType = RefType (Just Null) (AbsHeapType_HeapType NoExtern_AbsHeapType)

-- ### 6.4.5 Value Types

data ValType
  = NumType_ValType NumType
  | VecType_ValType VecType
  | RefType_ValType RefType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ValType where
  encodeWat (NumType_ValType nt) = encodeWat nt
  encodeWat (VecType_ValType vt) = encodeWat vt
  encodeWat (RefType_ValType rt) = encodeWat rt

-- ### 6.4.6 Composite Types

-- [TODO]

-- ### 6.4.7 Recursive Types

-- [TODO]

-- ### 6.4.8 Address Types

-- [TODO]

-- ### 6.4.9 Limits

-- [TODO]

-- ### 6.4.10 Tag Types

-- [TODO]

-- ### 6.4.11 Global Types

-- [TODO]

-- ### 6.4.12 Memory Types

-- [TODO]

-- ### 6.4.13 Table Types

-- [TODO]

-- ### 6.4.14 External Types

-- [TODO]

-- ### 6.4.15 Type Uses

-- [TODO]

-- ## 6.5 Instructions

-- ### 6.5.1 Labels

-- [TODO]

-- ### 6.5.2 Parametric Instructions

-- [TODO]

-- ### 6.5.3 Control Instructions

-- [TODO]

-- ### 6.5.4 Variable Instructions

-- [TODO]

-- ### 6.5.5 Table Instructions

-- [TODO]

-- ### 6.5.6 Memory Instructions

-- [TODO]

-- ### 6.5.7 Reference Instructions

-- [TODO]

-- ### 6.5.8 Aggregate Instructions

-- [TODO]

-- ### 6.5.9 Numeric Instructions

-- [TODO]

-- ### 6.5.10 Vector Instructions

-- [TODO]

-- ### 6.5.11 Folded Instructions

-- [TODO]

-- ### Expressions

-- [TODO]

-- ## 6.6 Modules

-- ### 6.6.1 Indices

data Idx = Idx Natural
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Idx where
  encodeWat (Idx n) = pretty n

data TypeIdx = TypeIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TypeIdx where
  encodeWat (TypeIdx i) = encodeWat i

data FuncIdx = FuncIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat FuncIdx where
  encodeWat (FuncIdx i) = encodeWat i

data GlobalIdx = GlobalIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat GlobalIdx where
  encodeWat (GlobalIdx i) = encodeWat i

data TableIdx = TableIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TableIdx where
  encodeWat (TableIdx i) = encodeWat i

data MemIdx = MemIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat MemIdx where
  encodeWat (MemIdx i) = encodeWat i

data TagIdx = TagIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TagIdx where
  encodeWat (TagIdx i) = encodeWat i

data ElemIdx = ElemIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ElemIdx where
  encodeWat (ElemIdx i) = encodeWat i

data DataIdx = DataIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat DataIdx where
  encodeWat (DataIdx i) = encodeWat i

data LabelIdx = LabelIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat LabelIdx where
  encodeWat (LabelIdx i) = encodeWat i

data LocalIdx = LocalIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat LocalIdx where
  encodeWat (LocalIdx i) = encodeWat i

data FieldIdx = FieldIdx Idx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat FieldIdx where
  encodeWat (FieldIdx i) = encodeWat i

-- ### 6.6.2 Types

-- [TODO]

-- ### 6.6.3 Tags

-- [TODO]

-- ### 6.6.4 Globals

-- [TODO]

-- ### 6.6.5 Memories

-- [TODO]

-- ### 6.6.6 Tables

-- [TODO]

-- ### 6.6.7 Functions

-- [TODO]

-- ### 6.6.8 Data Segments

-- [TODO]

-- ### 6.6.9 Element Segments

-- [TODO]

-- ### 6.6.10 Start Functions

-- [TODO]

-- ### 6.6.11 Imports

-- [TODO]

-- ### 6.6.12 Exports

-- [TODO]

-- ### 6.6.13 Modules
