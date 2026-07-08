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
  encodeWat (Annot i ts) = parens $ "@" <> encodeWat i <+> (hcat . punctuate " " . fmap encodeWat) ts

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
  encodeWat NoFunc_AbsHeapType = "nofunc"
  encodeWat Exn_AbsHeapType = "exn"
  encodeWat NoExn_AbsHeapType = "noexn"
  encodeWat Extern_AbsHeapType = "extern"
  encodeWat NoExtern_AbsHeapType = "noextern"

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
  encodeWat (RefType nullM ht) = parens $ "ref" <+> maybe mempty encodeWat nullM <+> encodeWat ht

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

-- | Composite types are parsed into their respective abstract representation, paired with the local identifier context generated by their bound field or parameter identifiers.
data CompType
  = Struct_CompType [Field]
  | Array_CompType FieldType
  | Func_CompType [Param] [Result]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat CompType where
  encodeWat (Struct_CompType fs) = parens $ "struct" <> hcat ((punctuate " " . fmap encodeWat) fs)
  encodeWat (Array_CompType ft) = parens $ "array" <+> encodeWat ft
  encodeWat (Func_CompType ps rs) = parens $ "func" <> hcat ((punctuate " " . fmap encodeWat) ps) <> hcat ((punctuate " " . fmap encodeWat) rs)

-- | A field declaration with an optional identifier and field type.
data Field = Field (Maybe Identifier) FieldType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Field where
  encodeWat (Field idM ft) = parens $ "field" <+> maybe mempty encodeWat idM <+> encodeWat ft

-- | A parameter declaration with an optional identifier and value type.
data Param = Param (Maybe Identifier) ValType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Param where
  encodeWat (Param idM vt) = parens $ "param" <+> maybe mempty encodeWat idM <+> encodeWat vt

-- | A result declaration specifying a value type.
newtype Result = Result ValType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Result where
  encodeWat (Result vt) = parens $ "result" <+> encodeWat vt

-- | Field types can be mutable or immutable.
data FieldType = FieldType Bool StorageType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat FieldType where
  encodeWat (FieldType isMut st) =
    if isMut
      then parens $ "mut" <+> encodeWat st
      else encodeWat st

-- | Storage types represent either value types or packed types.
data StorageType
  = ValType_StorageType ValType
  | PackType_StorageType PackType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat StorageType where
  encodeWat (ValType_StorageType vt) = encodeWat vt
  encodeWat (PackType_StorageType pt) = encodeWat pt

-- | Pack types represent i8 or i16 types.
data PackType
  = I8_PackType
  | I16_PackType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat PackType where
  encodeWat I8_PackType = "i8"
  encodeWat I16_PackType = "i16"

-- ### 6.4.7 Recursive Types

-- | Final types represent completed types.
data Final = Final
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Final where
  encodeWat Final = "final"

-- | Sub types specify inheritance relationships and composite types.
data SubType = SubType (Maybe Final) [TypeIdx] CompType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat SubType where
  encodeWat (SubType fM tis ct) = "(" <> "sub" <+> maybe mempty encodeWat fM <> hcat ((punctuate " " . fmap encodeWat) tis) <+> encodeWat ct <> ")"

-- | Type definitions declare custom types with optional identifiers.
data TypeDef = TypeDef (Maybe Identifier) SubType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TypeDef where
  encodeWat (TypeDef idM st) = parens $ "type" <+> maybe mempty encodeWat idM <+> encodeWat st

-- | Recursive types are parsed into their respective abstract representation.
data RecType = RecType [TypeDef]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat RecType where
  encodeWat (RecType tds) = "(" <> "rec" <> hcat ((punctuate " " . fmap encodeWat) tds) <> ")"

-- ### 6.4.8 Address Types

-- | Address types are either i32 or i64.
data AddrType
  = I32_AddrType
  | I64_AddrType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat AddrType where
  encodeWat I32_AddrType = "i32"
  encodeWat I64_AddrType = "i64"

-- ### 6.4.9 Limits

-- | Limits specify initial and optional maximum sizes.
data Limits = Limits Natural (Maybe Natural)
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Limits where
  encodeWat (Limits n mM) =
    pretty n <+> maybe mempty pretty mM

-- ### 6.4.10 Tag Types

-- | Tag types specify the type signature of exception or event tags.
newtype TagType = TagType TypeUse
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TagType where
  encodeWat (TagType tu) = encodeWat tu

-- ### 6.4.11 Global Types

-- | Global types specify the value type and mutability of a global variable.
data GlobalType = GlobalType Bool ValType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat GlobalType where
  encodeWat (GlobalType isMut vt) =
    if isMut
      then parens $ "mut" <+> encodeWat vt
      else encodeWat vt

-- ### 6.4.12 Memory Types

-- | Memory types specify address types and size limits.
data MemType = MemType (Maybe AddrType) Limits
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat MemType where
  encodeWat (MemType atM lims) =
    maybe mempty encodeWat atM <+> encodeWat lims

-- ### 6.4.13 Table Types

-- | Table types specify address types, limits, and reference types.
data TableType = TableType (Maybe AddrType) Limits RefType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TableType where
  encodeWat (TableType atM lims rt) =
    maybe mempty encodeWat atM <+> encodeWat lims <+> encodeWat rt

-- ### 6.4.14 External Types

-- | External types classify the kind of imports and exports.
data ExternalType
  = Tag_ExternalType (Maybe Identifier) TagType
  | Global_ExternalType (Maybe Identifier) GlobalType
  | Memory_ExternalType (Maybe Identifier) MemType
  | Table_ExternalType (Maybe Identifier) TableType
  | Func_ExternalType (Maybe Identifier) TypeUse
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ExternalType where
  encodeWat (Tag_ExternalType idM jt) = parens $ "tag" <+> maybe mempty encodeWat idM <+> encodeWat jt
  encodeWat (Global_ExternalType idM gt) = parens $ "global" <+> maybe mempty encodeWat idM <+> encodeWat gt
  encodeWat (Memory_ExternalType idM mt) = parens $ "memory" <+> maybe mempty encodeWat idM <+> encodeWat mt
  encodeWat (Table_ExternalType idM tt) = parens $ "table" <+> maybe mempty encodeWat idM <+> encodeWat tt
  encodeWat (Func_ExternalType idM tu) = parens $ "func" <+> maybe mempty encodeWat idM <+> encodeWat tu

-- ### 6.4.15 Type Uses

-- | Type uses reference type definitions, optionally inlining parameter and result declarations.
data TypeUse = TypeUse TypeIdx [Param] [Result]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TypeUse where
  encodeWat (TypeUse ti ps rs) = parens ("type" <+> encodeWat ti) <> hcat ((punctuate " " . fmap encodeWat) ps) <> hcat ((punctuate " " . fmap encodeWat) rs)

-- ## 6.5 Instructions

-- ### 6.5.1 Labels

-- | Structured control instructions can be annotated with an optional symbolic label identifier.
newtype Label = Label (Maybe Identifier)
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Label where
  encodeWat (Label idM) = maybe mempty encodeWat idM

-- ### 6.5.2 to 6.5.10 Instructions

-- | Block types are either result types or references to type definitions.
data BlockType
  = Result_BlockType (Maybe Result)
  | TypeUse_BlockType TypeUse
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat BlockType where
  encodeWat (Result_BlockType rM) = maybe mempty encodeWat rM
  encodeWat (TypeUse_BlockType tu) = encodeWat tu

-- | Structured catch clauses for exception handling.
data Catch
  = Catch TagIdx LabelIdx
  | CatchRef TagIdx LabelIdx
  | CatchAll LabelIdx
  | CatchAllRef LabelIdx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Catch where
  encodeWat (Catch tx l) = parens $ "catch" <+> encodeWat tx <+> encodeWat l
  encodeWat (CatchRef tx l) = parens $ "catch_ref" <+> encodeWat tx <+> encodeWat l
  encodeWat (CatchAll l) = parens $ "catch_all" <+> encodeWat l
  encodeWat (CatchAllRef l) = parens $ "catch_all_ref" <+> encodeWat l

-- | Structured control instructions.
data BlockInstr
  = Block_BlockInstr (Maybe Identifier) BlockType [Instr] (Maybe Identifier)
  | Loop_BlockInstr (Maybe Identifier) BlockType [Instr] (Maybe Identifier)
  | If_BlockInstr (Maybe Identifier) BlockType [Instr] (Maybe Identifier) [Instr] (Maybe Identifier)
  | TryTable_BlockInstr (Maybe Identifier) BlockType [Catch] [Instr] (Maybe Identifier)
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat BlockInstr where
  encodeWat (Block_BlockInstr idM bt body idEndM) = "block" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body)) <+> "end" <+> maybe mempty encodeWat idEndM
  encodeWat (Loop_BlockInstr idM bt body idEndM) = "loop" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body)) <+> "end" <+> maybe mempty encodeWat idEndM
  encodeWat (If_BlockInstr idM bt thenBody idElseM elseBody idEndM) = "if" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null thenBody then mempty else encodeWat (Instrs thenBody)) <> (if null elseBody then mempty else " else" <+> maybe mempty encodeWat idElseM <+> encodeWat (Instrs elseBody)) <+> "end" <+> maybe mempty encodeWat idEndM
  encodeWat (TryTable_BlockInstr idM bt cs body idEndM) = "try_table" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> hcat ((punctuate " " . fmap encodeWat) cs) <+> (if null body then mempty else encodeWat (Instrs body)) <+> "end" <+> maybe mempty encodeWat idEndM

-- | Memory instruction arguments (offset and alignment).
data MemArg = MemArg (Maybe Natural) (Maybe Natural)
  deriving (Generic, Eq, Show, Ord)

isMemArgEmpty :: MemArg -> Bool
isMemArgEmpty (MemArg Nothing Nothing) = True
isMemArgEmpty _ = False

instance EncodeWat MemArg where
  encodeWat (MemArg offsetM alignM) = hsep (foldMap (\o -> ["offset=" <> pretty o]) offsetM <> foldMap (\a -> ["align=" <> pretty a]) alignM)

-- | Lane index for SIMD instructions.
newtype LaneIdx = LaneIdx Natural
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat LaneIdx where
  encodeWat (LaneIdx n) = pretty n

-- | Plain instruction forms of WebAssembly.
data PlainInstr
  = Op0_PlainInstr Text
  | Select_PlainInstr (Maybe [Result])
  | Br_PlainInstr Text LabelIdx
  | BrTable_PlainInstr [LabelIdx] LabelIdx
  | Call_PlainInstr Text FuncIdx
  | CallIndirect_PlainInstr Text TableIdx TypeUse
  | Local_PlainInstr Text LocalIdx
  | Global_PlainInstr Text GlobalIdx
  | TableGet_PlainInstr Text TableIdx
  | TableCopy_PlainInstr TableIdx TableIdx
  | TableInit_PlainInstr TableIdx ElemIdx
  | ElemDrop_PlainInstr ElemIdx
  | Mem_PlainInstr Text MemIdx MemArg
  | MemLane_PlainInstr Text MemIdx MemArg LaneIdx
  | MemorySize_PlainInstr Text MemIdx
  | MemoryCopy_PlainInstr MemIdx MemIdx
  | MemoryInit_PlainInstr MemIdx DataIdx
  | DataDrop_PlainInstr DataIdx
  | RefNull_PlainInstr HeapType
  | RefFunc_PlainInstr FuncIdx
  | RefTest_PlainInstr RefType
  | RefCast_PlainInstr RefType
  | StructNew_PlainInstr Text TypeIdx
  | StructGet_PlainInstr Text TypeIdx FieldIdx
  | ArrayNew_PlainInstr Text TypeIdx
  | ArrayNewFixed_PlainInstr TypeIdx Natural
  | ArrayNewData_PlainInstr TypeIdx DataIdx
  | ArrayNewElem_PlainInstr TypeIdx ElemIdx
  | ArrayGet_PlainInstr Text TypeIdx
  | ArrayFill_PlainInstr TypeIdx
  | ArrayCopy_PlainInstr TypeIdx TypeIdx
  | ArrayInitData_PlainInstr TypeIdx DataIdx
  | ArrayInitElem_PlainInstr TypeIdx ElemIdx
  | I32Const_PlainInstr Integer
  | I64Const_PlainInstr Integer
  | F32Const_PlainInstr Float
  | F64Const_PlainInstr Float
  | V128Const_PlainInstr Text [Integer]
  | I8x16Shuffle_PlainInstr [LaneIdx]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat PlainInstr where
  encodeWat (Op0_PlainInstr op) = reflow op
  encodeWat (Select_PlainInstr rsM) = "select" <> maybe mempty (hcat . punctuate " " . fmap encodeWat) rsM
  encodeWat (Br_PlainInstr op l) = reflow op <+> encodeWat l
  encodeWat (BrTable_PlainInstr ls l) = "br_table" <+> hsep (fmap encodeWat ls) <+> encodeWat l
  encodeWat (Call_PlainInstr op f) = reflow op <+> encodeWat f
  encodeWat (CallIndirect_PlainInstr op t tu) = reflow op <+> encodeWat t <+> encodeWat tu
  encodeWat (Local_PlainInstr op l) = reflow op <+> encodeWat l
  encodeWat (Global_PlainInstr op g) = reflow op <+> encodeWat g
  encodeWat (TableGet_PlainInstr op t) = reflow op <+> encodeWat t
  encodeWat (TableCopy_PlainInstr t1 t2) = "table.copy" <+> encodeWat t1 <+> encodeWat t2
  encodeWat (TableInit_PlainInstr t e) = "table.init" <+> encodeWat t <+> encodeWat e
  encodeWat (ElemDrop_PlainInstr e) = "elem.drop" <+> encodeWat e
  encodeWat (Mem_PlainInstr op m arg) = reflow op <+> encodeWat m <+> (if isMemArgEmpty arg then mempty else encodeWat arg)
  encodeWat (MemLane_PlainInstr op m arg lane) = reflow op <+> encodeWat m <+> (if isMemArgEmpty arg then mempty else encodeWat arg) <+> encodeWat lane
  encodeWat (MemorySize_PlainInstr op m) = reflow op <+> encodeWat m
  encodeWat (MemoryCopy_PlainInstr m1 m2) = "memory.copy" <+> encodeWat m1 <+> encodeWat m2
  encodeWat (MemoryInit_PlainInstr m d) = "memory.init" <+> encodeWat m <+> encodeWat d
  encodeWat (DataDrop_PlainInstr d) = "data.drop" <+> encodeWat d
  encodeWat (RefNull_PlainInstr ht) = "ref.null" <+> encodeWat ht
  encodeWat (RefFunc_PlainInstr f) = "ref.func" <+> encodeWat f
  encodeWat (RefTest_PlainInstr rt) = "ref.test" <+> encodeWat rt
  encodeWat (RefCast_PlainInstr rt) = "ref.cast" <+> encodeWat rt
  encodeWat (StructNew_PlainInstr op t) = reflow op <+> encodeWat t
  encodeWat (StructGet_PlainInstr op t f) = reflow op <+> encodeWat t <+> encodeWat f
  encodeWat (ArrayNew_PlainInstr op t) = reflow op <+> encodeWat t
  encodeWat (ArrayNewFixed_PlainInstr t n) = "array.new_fixed" <+> encodeWat t <+> pretty n
  encodeWat (ArrayNewData_PlainInstr t d) = "array.new_data" <+> encodeWat t <+> encodeWat d
  encodeWat (ArrayNewElem_PlainInstr t e) = "array.new_elem" <+> encodeWat t <+> encodeWat e
  encodeWat (ArrayGet_PlainInstr op t) = reflow op <+> encodeWat t
  encodeWat (ArrayFill_PlainInstr t) = "array.fill" <+> encodeWat t
  encodeWat (ArrayCopy_PlainInstr t1 t2) = "array.copy" <+> encodeWat t1 <+> encodeWat t2
  encodeWat (ArrayInitData_PlainInstr t d) = "array.init_data" <+> encodeWat t <+> encodeWat d
  encodeWat (ArrayInitElem_PlainInstr t e) = "array.init_elem" <+> encodeWat t <+> encodeWat e
  encodeWat (I32Const_PlainInstr c) = "i32.const" <+> pretty c
  encodeWat (I64Const_PlainInstr c) = "i64.const" <+> pretty c
  encodeWat (F32Const_PlainInstr c) = "f32.const" <+> pretty c
  encodeWat (F64Const_PlainInstr c) = "f64.const" <+> pretty c
  encodeWat (V128Const_PlainInstr shape cs) = "v128.const" <+> reflow shape <+> hsep (fmap pretty cs)
  encodeWat (I8x16Shuffle_PlainInstr lanes) = "i8x16.shuffle" <+> hsep (fmap encodeWat lanes)

-- | General WebAssembly instructions.
data Instr
  = Plain_Instr PlainInstr
  | Block_Instr BlockInstr
  | Folded_Instr FoldedInstr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Instr where
  encodeWat (Plain_Instr p) = encodeWat p
  encodeWat (Block_Instr bi) = encodeWat bi
  encodeWat (Folded_Instr fi) = encodeWat fi

-- | Sequences of instructions.
newtype Instrs = Instrs [Instr]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Instrs where
  encodeWat (Instrs is) = hsep (fmap encodeWat is)

-- ### 6.5.11 Folded Instructions

-- | Folded instructions are syntactic sugar to group instructions visually.
data FoldedInstr
  = Plain_FoldedInstr PlainInstr [FoldedInstr]
  | Block_FoldedInstr (Maybe Identifier) BlockType [Instr]
  | Loop_FoldedInstr (Maybe Identifier) BlockType [Instr]
  | If_FoldedInstr (Maybe Identifier) BlockType [FoldedInstr] [Instr] [Instr]
  | TryTable_FoldedInstr (Maybe Identifier) BlockType [Catch] [Instr]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat FoldedInstr where
  encodeWat (Plain_FoldedInstr p inputs) = "(" <> encodeWat p <> hcat ((punctuate " " . fmap encodeWat) inputs) <> ")"
  encodeWat (Block_FoldedInstr idM bt body) = "(" <> "block" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body)) <> ")"
  encodeWat (Loop_FoldedInstr idM bt body) = "(" <> "loop" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body)) <> ")"
  encodeWat (If_FoldedInstr idM bt inputs thenBody elseBody) = "(" <> "if" <+> maybe mempty encodeWat idM <+> encodeWat bt <> hcat ((punctuate " " . fmap encodeWat) inputs) <+> "(" <> "then" <+> (if null thenBody then mempty else encodeWat (Instrs thenBody)) <> ")" <+> (if null elseBody then mempty else "(" <> "else" <+> encodeWat (Instrs elseBody) <> ")") <> ")"
  encodeWat (TryTable_FoldedInstr idM bt cs body) = "(" <> "try_table" <+> maybe mempty encodeWat idM <+> encodeWat bt <> hcat ((punctuate " " . fmap encodeWat) cs) <+> (if null body then mempty else encodeWat (Instrs body)) <> ")"

-- ### Expressions

-- | Expressions are written as instruction sequences.
newtype Expr = Expr [Instr]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Expr where
  encodeWat (Expr is) = encodeWat (Instrs is)

-- ## 6.6 Modules

-- ### 6.6.1 Indices

-- | Indices can be given in either numeric or symbolic form.
data Idx
  = Index_Idx Natural
  | Identifier_Idx Identifier
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Idx where
  encodeWat (Index_Idx n) = pretty n
  encodeWat (Identifier_Idx i) = encodeWat i

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

-- | A type definition consists of a recursive type.
newtype Type = Type RecType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Type where
  encodeWat (Type rt) = encodeWat rt

-- ### 6.6.3 Tags

-- | Tag definitions bind a symbolic tag identifier.
data Tag = Tag (Maybe Identifier) TagType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Tag where
  encodeWat (Tag idM tt) = parens $ "tag" <+> maybe mempty encodeWat idM <+> encodeWat tt

-- ### 6.6.4 Globals

-- | Global definitions bind a symbolic global identifier.
data Global = Global (Maybe Identifier) GlobalType Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Global where
  encodeWat (Global idM gt e) = parens $ "global" <+> maybe mempty encodeWat idM <+> encodeWat gt <+> encodeWat e

-- ### 6.6.5 Memories

-- | Memory definitions bind a symbolic memory identifier.
data Mem = Mem (Maybe Identifier) MemType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Mem where
  encodeWat (Mem idM mt) = parens $ "memory" <+> maybe mempty encodeWat idM <+> encodeWat mt

-- ### 6.6.6 Tables

-- | Table definitions bind a symbolic table identifier.
data Table = Table (Maybe Identifier) TableType Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Table where
  encodeWat (Table idM tt e) = parens $ "table" <+> maybe mempty encodeWat idM <+> encodeWat tt <+> encodeWat e

-- ### 6.6.7 Functions

-- | Local variables in a function.
data Local = Local (Maybe Identifier) ValType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Local where
  encodeWat (Local idM vt) = parens $ "local" <+> maybe mempty encodeWat idM <+> encodeWat vt

-- | Function definitions bind a symbolic function identifier and local identifiers for its parameters and locals.
data Func = Func (Maybe Identifier) TypeUse [Local] Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Func where
  encodeWat (Func idM tu ls e) = "(" <> "func" <+> maybe mempty encodeWat idM <+> encodeWat tu <> hcat ((punctuate " " . fmap encodeWat) ls) <+> encodeWat e <> ")"

-- ### 6.6.8 Data Segments

-- | Data segments can be active or passive.
data DataMode
  = Passive_DataMode
  | Active_DataMode MemIdx Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat DataMode where
  encodeWat Passive_DataMode = mempty
  encodeWat (Active_DataMode x e) = parens ("memory" <+> encodeWat x) <+> parens ("offset" <+> encodeWat e)

-- | Data segments allow for an optional memory index to identify the memory to initialize.
data DataSegment = DataSegment (Maybe Identifier) DataMode ByteString
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat DataSegment where
  encodeWat (DataSegment idM mode bs) = parens $ "data" <+> maybe mempty encodeWat idM <+> encodeWat mode <+> encodeWat bs

-- ### 6.6.9 Element Segments

-- | Element segment initialization mode.
data ElemMode
  = Passive_ElemMode
  | Active_ElemMode TableIdx Expr
  | Declare_ElemMode
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ElemMode where
  encodeWat Passive_ElemMode = mempty
  encodeWat (Active_ElemMode x e) = parens ("table" <+> encodeWat x) <+> parens ("offset" <+> encodeWat e)
  encodeWat Declare_ElemMode = "declare"

-- | Individual element expressions.
newtype ElemExpr = ElemExpr Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ElemExpr where
  encodeWat (ElemExpr e) = parens $ "item" <+> encodeWat e

-- | A list of elements with reference type.
data ElemList = ElemList RefType [ElemExpr]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ElemList where
  encodeWat (ElemList rt es) =
    encodeWat rt <> hcat ((punctuate " " . fmap encodeWat) es)

-- | Element segments allow for an optional table index to identify the table to initialize.
data ElemSegment = ElemSegment (Maybe Identifier) ElemMode ElemList
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ElemSegment where
  encodeWat (ElemSegment idM mode el) = parens $ "elem" <+> maybe mempty encodeWat idM <+> encodeWat mode <+> encodeWat el

-- ### 6.6.10 Start Functions

-- | A start function is defined in terms of its index.
newtype Start = Start FuncIdx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Start where
  encodeWat (Start f) = parens $ "start" <+> encodeWat f

-- ### 6.6.11 Imports

-- | Imports define the external module, name, and type category.
data Import = Import Name Name ExternalType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Import where
  encodeWat (Import nm1 nm2 et) =
    parens $ "import" <+> encodeWat nm1 <+> encodeWat nm2 <+> encodeWat et

-- ### 6.6.12 Exports

-- | Export category and target index.
data ExternIdx
  = Tag_ExternIdx TagIdx
  | Global_ExternIdx GlobalIdx
  | Memory_ExternIdx MemIdx
  | Table_ExternIdx TableIdx
  | Func_ExternIdx FuncIdx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat ExternIdx where
  encodeWat (Tag_ExternIdx x) = parens $ "tag" <+> encodeWat x
  encodeWat (Global_ExternIdx x) = parens $ "global" <+> encodeWat x
  encodeWat (Memory_ExternIdx x) = parens $ "memory" <+> encodeWat x
  encodeWat (Table_ExternIdx x) = parens $ "table" <+> encodeWat x
  encodeWat (Func_ExternIdx x) = parens $ "func" <+> encodeWat x

-- | Exports reveal definitions to the outside environment.
data Export = Export Name ExternIdx
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Export where
  encodeWat (Export nm xx) = parens $ "export" <+> encodeWat nm <+> encodeWat xx

-- ### 6.6.13 Modules

-- | Module declarations.
data Decl
  = Type_Decl Type
  | Import_Decl Import
  | Tag_Decl Tag
  | Global_Decl Global
  | Mem_Decl Mem
  | Table_Decl Table
  | Func_Decl Func
  | Data_Decl DataSegment
  | Elem_Decl ElemSegment
  | Start_Decl Start
  | Export_Decl Export
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Decl where
  encodeWat (Type_Decl t) = encodeWat t
  encodeWat (Import_Decl i) = encodeWat i
  encodeWat (Tag_Decl t) = encodeWat t
  encodeWat (Global_Decl g) = encodeWat g
  encodeWat (Mem_Decl m) = encodeWat m
  encodeWat (Table_Decl t) = encodeWat t
  encodeWat (Func_Decl f) = encodeWat f
  encodeWat (Data_Decl d) = encodeWat d
  encodeWat (Elem_Decl e) = encodeWat e
  encodeWat (Start_Decl s) = encodeWat s
  encodeWat (Export_Decl e) = encodeWat e

-- | A module consists of a sequence of declarations.
data Module = Module (Maybe Identifier) [Decl]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Module where
  encodeWat (Module idM decls) = parens $ "module" <+> maybe mempty encodeWat idM <+> (if null decls then mempty else hsep (fmap encodeWat decls))
