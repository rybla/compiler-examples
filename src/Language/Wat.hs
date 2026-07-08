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

data Sexp = Sexp Text [Sexp]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Sexp where
  encodeWat (Sexp t es)
    | null es = reflow t
    | otherwise = parens $ reflow t <+> (hcat . punctuate " " . fmap encodeWat) es

class EncodeWatSexp a where
  encodeWatSexp :: a -> Sexp

--------------------------------

-- # 6 WebAssembly Text Format (WAT)

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
  encodeWat (Name t) = reflow . Text.show $ t

-- ### 6.3.5 Identifiers

-- | Indices can be given in both numeric and symbolic form. Symbolic identifiers that stand in lieu of indices start with "$".
data Identifier = Identifier Text
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Identifier where
  encodeWat (Identifier t) = reflow . Text.show $ t

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
  encodeWat (Struct_CompType fs) = parens $ "struct" <+> (hcat . punctuate " " . fmap encodeWat) fs
  encodeWat (Array_CompType ft) = parens $ "array" <+> encodeWat ft
  encodeWat (Func_CompType ps rs) = parens $ "func" <+> (hcat . punctuate " " . fmap encodeWat) ps <+> (hcat . punctuate " " . fmap encodeWat) rs

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
  encodeWat (SubType fM tis ct) = parens $ "sub" <+> maybe mempty encodeWat fM <+> (hcat . punctuate " " . fmap encodeWat) tis <+> encodeWat ct

-- | Type definitions declare custom types with optional identifiers.
data TypeDef = TypeDef (Maybe Identifier) SubType
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat TypeDef where
  encodeWat (TypeDef idM st) = parens $ "type" <+> maybe mempty encodeWat idM <+> encodeWat st

-- | Recursive types are parsed into their respective abstract representation.
data RecType = RecType [TypeDef]
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat RecType where
  encodeWat (RecType tds) = parens $ "rec" <+> (hcat . punctuate " " . fmap encodeWat) tds

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
  encodeWat (TypeUse ti ps rs) = parens ("type" <+> encodeWat ti) <+> (hcat . punctuate " " . fmap encodeWat) ps <+> (hcat . punctuate " " . fmap encodeWat) rs

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
  encodeWat (If_BlockInstr idM bt thenBody idElseM elseBody idEndM) = "if" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null thenBody then mempty else encodeWat (Instrs thenBody)) <+> (if null elseBody then mempty else " else" <+> maybe mempty encodeWat idElseM <+> encodeWat (Instrs elseBody)) <+> "end" <+> maybe mempty encodeWat idEndM
  encodeWat (TryTable_BlockInstr idM bt cs body idEndM) = "try_table" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (hcat . punctuate " " . fmap encodeWat) cs <+> (if null body then mempty else encodeWat (Instrs body)) <+> "end" <+> maybe mempty encodeWat idEndM

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
  = -- ### 6.5.2 Parametric Instructions
    Unreachable_PlainInstr
  | Nop_PlainInstr
  | Drop_PlainInstr
  | Select_PlainInstr (Maybe [Result])
  | -- ### 6.5.3 Plain Control Instructions
    Br_PlainInstr LabelIdx
  | BrIf_PlainInstr LabelIdx
  | BrTable_PlainInstr [LabelIdx] LabelIdx
  | Return_PlainInstr
  | Call_PlainInstr FuncIdx
  | ReturnCall_PlainInstr FuncIdx
  | CallIndirect_PlainInstr TableIdx TypeUse
  | ReturnCallIndirect_PlainInstr TableIdx TypeUse
  | -- ### 6.5.4 Variable Instructions
    LocalGet_PlainInstr LocalIdx
  | LocalSet_PlainInstr LocalIdx
  | LocalTee_PlainInstr LocalIdx
  | GlobalGet_PlainInstr GlobalIdx
  | GlobalSet_PlainInstr GlobalIdx
  | -- ### 6.5.5 Table Instructions
    TableGet_PlainInstr TableIdx
  | TableSet_PlainInstr TableIdx
  | TableSize_PlainInstr TableIdx
  | TableGrow_PlainInstr TableIdx
  | TableFill_PlainInstr TableIdx
  | TableCopy_PlainInstr TableIdx TableIdx
  | TableInit_PlainInstr TableIdx ElemIdx
  | ElemDrop_PlainInstr ElemIdx
  | -- ### 6.5.6 Memory Instructions
    I32Load_PlainInstr MemIdx MemArg
  | I64Load_PlainInstr MemIdx MemArg
  | F32Load_PlainInstr MemIdx MemArg
  | F64Load_PlainInstr MemIdx MemArg
  | I32Load8S_PlainInstr MemIdx MemArg
  | I32Load8U_PlainInstr MemIdx MemArg
  | I32Load16S_PlainInstr MemIdx MemArg
  | I32Load16U_PlainInstr MemIdx MemArg
  | I64Load8S_PlainInstr MemIdx MemArg
  | I64Load8U_PlainInstr MemIdx MemArg
  | I64Load16S_PlainInstr MemIdx MemArg
  | I64Load16U_PlainInstr MemIdx MemArg
  | I64Load32S_PlainInstr MemIdx MemArg
  | I64Load32U_PlainInstr MemIdx MemArg
  | V128Load_PlainInstr MemIdx MemArg
  | V128Load8x8S_PlainInstr MemIdx MemArg
  | V128Load8x8U_PlainInstr MemIdx MemArg
  | V128Load16x4S_PlainInstr MemIdx MemArg
  | V128Load16x4U_PlainInstr MemIdx MemArg
  | V128Load32x2S_PlainInstr MemIdx MemArg
  | V128Load32x2U_PlainInstr MemIdx MemArg
  | V128Load8Splat_PlainInstr MemIdx MemArg
  | V128Load16Splat_PlainInstr MemIdx MemArg
  | V128Load32Splat_PlainInstr MemIdx MemArg
  | V128Load64Splat_PlainInstr MemIdx MemArg
  | V128Load32Zero_PlainInstr MemIdx MemArg
  | V128Load64Zero_PlainInstr MemIdx MemArg
  | V128Load8Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Load16Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Load32Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Load64Lane_PlainInstr MemIdx MemArg LaneIdx
  | I32Store_PlainInstr MemIdx MemArg
  | I64Store_PlainInstr MemIdx MemArg
  | F32Store_PlainInstr MemIdx MemArg
  | F64Store_PlainInstr MemIdx MemArg
  | I32Store8_PlainInstr MemIdx MemArg
  | I32Store16_PlainInstr MemIdx MemArg
  | I64Store8_PlainInstr MemIdx MemArg
  | I64Store16_PlainInstr MemIdx MemArg
  | I64Store32_PlainInstr MemIdx MemArg
  | V128Store_PlainInstr MemIdx MemArg
  | V128Store8Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Store16Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Store32Lane_PlainInstr MemIdx MemArg LaneIdx
  | V128Store64Lane_PlainInstr MemIdx MemArg LaneIdx
  | MemorySize_PlainInstr MemIdx
  | MemoryGrow_PlainInstr MemIdx
  | MemoryFill_PlainInstr MemIdx
  | MemoryCopy_PlainInstr MemIdx MemIdx
  | MemoryInit_PlainInstr MemIdx DataIdx
  | DataDrop_PlainInstr DataIdx
  | -- ### 6.5.7 Reference Instructions
    RefNull_PlainInstr HeapType
  | RefFunc_PlainInstr FuncIdx
  | RefIsNull_PlainInstr
  | RefAsNonNull_PlainInstr
  | RefEq_PlainInstr
  | RefTest_PlainInstr RefType
  | RefCast_PlainInstr RefType
  | -- ### 6.5.8 Aggregate Instructions
    RefI31_PlainInstr
  | I31GetS_PlainInstr
  | I31GetU_PlainInstr
  | StructNew_PlainInstr TypeIdx
  | StructNewDefault_PlainInstr TypeIdx
  | StructGet_PlainInstr TypeIdx FieldIdx
  | StructGetS_PlainInstr TypeIdx FieldIdx
  | StructGetU_PlainInstr TypeIdx FieldIdx
  | StructSet_PlainInstr TypeIdx FieldIdx
  | ArrayNew_PlainInstr TypeIdx
  | ArrayNewDefault_PlainInstr TypeIdx
  | ArrayNewFixed_PlainInstr TypeIdx Natural
  | ArrayNewData_PlainInstr TypeIdx DataIdx
  | ArrayNewElem_PlainInstr TypeIdx ElemIdx
  | ArrayGet_PlainInstr TypeIdx
  | ArrayGetS_PlainInstr TypeIdx
  | ArrayGetU_PlainInstr TypeIdx
  | ArraySet_PlainInstr TypeIdx
  | ArrayLen_PlainInstr
  | ArrayFill_PlainInstr TypeIdx
  | ArrayCopy_PlainInstr TypeIdx TypeIdx
  | ArrayInitData_PlainInstr TypeIdx DataIdx
  | ArrayInitElem_PlainInstr TypeIdx ElemIdx
  | AnyConvertExtern_PlainInstr
  | ExternConvertAny_PlainInstr
  | -- ### 6.5.9 Numeric Const Instructions
    I32Const_PlainInstr Integer
  | I64Const_PlainInstr Integer
  | F32Const_PlainInstr Float
  | F64Const_PlainInstr Float
  | -- ### 6.5.9 Numeric Operators
    -- i32 operators
    I32Eqz_PlainInstr
  | I32Eq_PlainInstr
  | I32Ne_PlainInstr
  | I32LtS_PlainInstr
  | I32LtU_PlainInstr
  | I32GtS_PlainInstr
  | I32GtU_PlainInstr
  | I32LeS_PlainInstr
  | I32LeU_PlainInstr
  | I32GeS_PlainInstr
  | I32GeU_PlainInstr
  | I32Clz_PlainInstr
  | I32Ctz_PlainInstr
  | I32Popcnt_PlainInstr
  | I32Extend8S_PlainInstr
  | I32Extend16S_PlainInstr
  | I32Add_PlainInstr
  | I32Sub_PlainInstr
  | I32Mul_PlainInstr
  | I32DivS_PlainInstr
  | I32DivU_PlainInstr
  | I32RemS_PlainInstr
  | I32RemU_PlainInstr
  | I32And_PlainInstr
  | I32Or_PlainInstr
  | I32Xor_PlainInstr
  | I32Shl_PlainInstr
  | I32ShrS_PlainInstr
  | I32ShrU_PlainInstr
  | I32Rotl_PlainInstr
  | I32Rotr_PlainInstr
  | -- i64 operators
    I64Eqz_PlainInstr
  | I64Eq_PlainInstr
  | I64Ne_PlainInstr
  | I64LtS_PlainInstr
  | I64LtU_PlainInstr
  | I64GtS_PlainInstr
  | I64GtU_PlainInstr
  | I64LeS_PlainInstr
  | I64LeU_PlainInstr
  | I64GeS_PlainInstr
  | I64GeU_PlainInstr
  | I64Clz_PlainInstr
  | I64Ctz_PlainInstr
  | I64Popcnt_PlainInstr
  | I64Extend8S_PlainInstr
  | I64Extend16S_PlainInstr
  | I64Extend32S_PlainInstr
  | I64Add_PlainInstr
  | I64Sub_PlainInstr
  | I64Mul_PlainInstr
  | I64DivS_PlainInstr
  | I64DivU_PlainInstr
  | I64RemS_PlainInstr
  | I64RemU_PlainInstr
  | I64And_PlainInstr
  | I64Or_PlainInstr
  | I64Xor_PlainInstr
  | I64Shl_PlainInstr
  | I64ShrS_PlainInstr
  | I64ShrU_PlainInstr
  | I64Rotl_PlainInstr
  | I64Rotr_PlainInstr
  | -- f32 operators
    F32Eq_PlainInstr
  | F32Ne_PlainInstr
  | F32Lt_PlainInstr
  | F32Gt_PlainInstr
  | F32Le_PlainInstr
  | F32Ge_PlainInstr
  | F32Abs_PlainInstr
  | F32Neg_PlainInstr
  | F32Sqrt_PlainInstr
  | F32Ceil_PlainInstr
  | F32Floor_PlainInstr
  | F32Trunc_PlainInstr
  | F32Nearest_PlainInstr
  | F32Add_PlainInstr
  | F32Sub_PlainInstr
  | F32Mul_PlainInstr
  | F32Div_PlainInstr
  | F32Min_PlainInstr
  | F32Max_PlainInstr
  | F32Copysign_PlainInstr
  | -- f64 operators
    F64Eq_PlainInstr
  | F64Ne_PlainInstr
  | F64Lt_PlainInstr
  | F64Gt_PlainInstr
  | F64Le_PlainInstr
  | F64Ge_PlainInstr
  | F64Abs_PlainInstr
  | F64Neg_PlainInstr
  | F64Sqrt_PlainInstr
  | F64Ceil_PlainInstr
  | F64Floor_PlainInstr
  | F64Trunc_PlainInstr
  | F64Nearest_PlainInstr
  | F64Add_PlainInstr
  | F64Sub_PlainInstr
  | F64Mul_PlainInstr
  | F64Div_PlainInstr
  | F64Min_PlainInstr
  | F64Max_PlainInstr
  | F64Copysign_PlainInstr
  | -- conversions
    I32WrapI64_PlainInstr
  | I32TruncF32S_PlainInstr
  | I32TruncF32U_PlainInstr
  | I32TruncF64S_PlainInstr
  | I32TruncF64U_PlainInstr
  | I32TruncSatF32S_PlainInstr
  | I32TruncSatF32U_PlainInstr
  | I32TruncSatF64S_PlainInstr
  | I32TruncSatF64U_PlainInstr
  | I64ExtendI32S_PlainInstr
  | I64ExtendI32U_PlainInstr
  | I64TruncF32S_PlainInstr
  | I64TruncF32U_PlainInstr
  | I64TruncF64S_PlainInstr
  | I64TruncF64U_PlainInstr
  | I64TruncSatF32S_PlainInstr
  | I64TruncSatF32U_PlainInstr
  | I64TruncSatF64S_PlainInstr
  | I64TruncSatF64U_PlainInstr
  | F32DemoteF64_PlainInstr
  | F32ConvertI32S_PlainInstr
  | F32ConvertI32U_PlainInstr
  | F32ConvertI64S_PlainInstr
  | F32ConvertI64U_PlainInstr
  | F64PromoteF32_PlainInstr
  | F64ConvertI32S_PlainInstr
  | F64ConvertI32U_PlainInstr
  | F64ConvertI64S_PlainInstr
  | F64ConvertI64U_PlainInstr
  | I32ReinterpretF32_PlainInstr
  | I64ReinterpretF64_PlainInstr
  | F32ReinterpretI32_PlainInstr
  | F64ReinterpretI64_PlainInstr
  | -- ### 6.5.10 Vector Instructions
    -- vector const and shuffle
    V128Const_PlainInstr Text [Integer]
  | I8x16Shuffle_PlainInstr [LaneIdx]
  | -- swizzle and splats
    I8x16Swizzle_PlainInstr
  | I8x16RelaxedSwizzle_PlainInstr
  | I8x16Splat_PlainInstr
  | I16x8Splat_PlainInstr
  | I32x4Splat_PlainInstr
  | I64x2Splat_PlainInstr
  | F32x4Splat_PlainInstr
  | F64x2Splat_PlainInstr
  | -- lanes
    I8x16ExtractLaneS_PlainInstr LaneIdx
  | I8x16ExtractLaneU_PlainInstr LaneIdx
  | I16x8ExtractLaneS_PlainInstr LaneIdx
  | I16x8ExtractLaneU_PlainInstr LaneIdx
  | I32x4ExtractLane_PlainInstr LaneIdx
  | I64x2ExtractLane_PlainInstr LaneIdx
  | F32x4ExtractLane_PlainInstr LaneIdx
  | F64x2ExtractLane_PlainInstr LaneIdx
  | I8x16ReplaceLane_PlainInstr LaneIdx
  | I16x8ReplaceLane_PlainInstr LaneIdx
  | I32x4ReplaceLane_PlainInstr LaneIdx
  | I64x2ReplaceLane_PlainInstr LaneIdx
  | F32x4ReplaceLane_PlainInstr LaneIdx
  | F64x2ReplaceLane_PlainInstr LaneIdx
  | -- vector ops
    V128AnyTrue_PlainInstr
  | V128Not_PlainInstr
  | V128And_PlainInstr
  | V128Andnot_PlainInstr
  | V128Or_PlainInstr
  | V128Xor_PlainInstr
  | V128Bitselect_PlainInstr
  | I8x16AllTrue_PlainInstr
  | I8x16Eq_PlainInstr
  | I8x16Ne_PlainInstr
  | I8x16LtS_PlainInstr
  | I8x16LtU_PlainInstr
  | I8x16GtS_PlainInstr
  | I8x16GtU_PlainInstr
  | I8x16LeS_PlainInstr
  | I8x16LeU_PlainInstr
  | I8x16GeS_PlainInstr
  | I8x16GeU_PlainInstr
  | I8x16Abs_PlainInstr
  | I8x16Neg_PlainInstr
  | I8x16Popcnt_PlainInstr
  | I8x16Add_PlainInstr
  | I8x16AddSatS_PlainInstr
  | I8x16AddSatU_PlainInstr
  | I8x16Sub_PlainInstr
  | I8x16SubSatS_PlainInstr
  | I8x16SubSatU_PlainInstr
  | I8x16MinS_PlainInstr
  | I8x16MinU_PlainInstr
  | I8x16MaxS_PlainInstr
  | I8x16MaxU_PlainInstr
  | I8x16AvgrU_PlainInstr
  | I8x16RelaxedLaneselect_PlainInstr
  | I8x16Shl_PlainInstr
  | I8x16ShrS_PlainInstr
  | I8x16ShrU_PlainInstr
  | I8x16Bitmask_PlainInstr
  | I8x16NarrowI16x8S_PlainInstr
  | I8x16NarrowI16x8U_PlainInstr
  | -- i16x8 ops
    I16x8AllTrue_PlainInstr
  | I16x8Eq_PlainInstr
  | I16x8Ne_PlainInstr
  | I16x8LtS_PlainInstr
  | I16x8LtU_PlainInstr
  | I16x8GtS_PlainInstr
  | I16x8GtU_PlainInstr
  | I16x8LeS_PlainInstr
  | I16x8LeU_PlainInstr
  | I16x8GeS_PlainInstr
  | I16x8GeU_PlainInstr
  | I16x8Abs_PlainInstr
  | I16x8Neg_PlainInstr
  | I16x8Add_PlainInstr
  | I16x8AddSatS_PlainInstr
  | I16x8AddSatU_PlainInstr
  | I16x8Sub_PlainInstr
  | I16x8SubSatS_PlainInstr
  | I16x8SubSatU_PlainInstr
  | I16x8Mul_PlainInstr
  | I16x8MinS_PlainInstr
  | I16x8MinU_PlainInstr
  | I16x8MaxS_PlainInstr
  | I16x8MaxU_PlainInstr
  | I16x8AvgrU_PlainInstr
  | I16x8Q15mulrSatS_PlainInstr
  | I16x8RelaxedQ15mulrS_PlainInstr
  | I16x8RelaxedLaneselect_PlainInstr
  | I16x8Shl_PlainInstr
  | I16x8ShrS_PlainInstr
  | I16x8ShrU_PlainInstr
  | I16x8Bitmask_PlainInstr
  | I16x8NarrowI32x4S_PlainInstr
  | I16x8NarrowI32x4U_PlainInstr
  | -- i32x4 ops
    I32x4AllTrue_PlainInstr
  | I32x4Eq_PlainInstr
  | I32x4Ne_PlainInstr
  | I32x4LtS_PlainInstr
  | I32x4LtU_PlainInstr
  | I32x4GtS_PlainInstr
  | I32x4GtU_PlainInstr
  | I32x4LeS_PlainInstr
  | I32x4LeU_PlainInstr
  | I32x4GeS_PlainInstr
  | I32x4GeU_PlainInstr
  | I32x4Abs_PlainInstr
  | I32x4Neg_PlainInstr
  | I32x4Add_PlainInstr
  | I32x4Sub_PlainInstr
  | I32x4Mul_PlainInstr
  | I32x4MinS_PlainInstr
  | I32x4MinU_PlainInstr
  | I32x4MaxS_PlainInstr
  | I32x4MaxU_PlainInstr
  | I32x4RelaxedLaneselect_PlainInstr
  | I32x4Shl_PlainInstr
  | I32x4ShrS_PlainInstr
  | I32x4ShrU_PlainInstr
  | I32x4Bitmask_PlainInstr
  | -- i64x2 ops
    I64x2AllTrue_PlainInstr
  | I64x2Eq_PlainInstr
  | I64x2Ne_PlainInstr
  | I64x2LtS_PlainInstr
  | I64x2GtS_PlainInstr
  | I64x2LeS_PlainInstr
  | I64x2GeS_PlainInstr
  | I64x2Abs_PlainInstr
  | I64x2Neg_PlainInstr
  | I64x2Add_PlainInstr
  | I64x2Sub_PlainInstr
  | I64x2Mul_PlainInstr
  | I64x2RelaxedLaneselect_PlainInstr
  | I64x2Shl_PlainInstr
  | I64x2ShrS_PlainInstr
  | I64x2ShrU_PlainInstr
  | I64x2Bitmask_PlainInstr
  | -- f32x4 ops
    F32x4Eq_PlainInstr
  | F32x4Ne_PlainInstr
  | F32x4Lt_PlainInstr
  | F32x4Gt_PlainInstr
  | F32x4Le_PlainInstr
  | F32x4Ge_PlainInstr
  | F32x4Abs_PlainInstr
  | F32x4Neg_PlainInstr
  | F32x4Sqrt_PlainInstr
  | F32x4Ceil_PlainInstr
  | F32x4Floor_PlainInstr
  | F32x4Trunc_PlainInstr
  | F32x4Nearest_PlainInstr
  | F32x4Add_PlainInstr
  | F32x4Sub_PlainInstr
  | F32x4Mul_PlainInstr
  | F32x4Div_PlainInstr
  | F32x4Min_PlainInstr
  | F32x4Max_PlainInstr
  | F32x4Pmin_PlainInstr
  | F32x4Pmax_PlainInstr
  | F32x4RelaxedMin_PlainInstr
  | F32x4RelaxedMax_PlainInstr
  | F32x4RelaxedMadd_PlainInstr
  | F32x4RelaxedNmadd_PlainInstr
  | -- f64x2 ops
    F64x2Eq_PlainInstr
  | F64x2Ne_PlainInstr
  | F64x2Lt_PlainInstr
  | F64x2Gt_PlainInstr
  | F64x2Le_PlainInstr
  | F64x2Ge_PlainInstr
  | F64x2Abs_PlainInstr
  | F64x2Neg_PlainInstr
  | F64x2Sqrt_PlainInstr
  | F64x2Ceil_PlainInstr
  | F64x2Floor_PlainInstr
  | F64x2Trunc_PlainInstr
  | F64x2Nearest_PlainInstr
  | F64x2Add_PlainInstr
  | F64x2Sub_PlainInstr
  | F64x2Mul_PlainInstr
  | F64x2Div_PlainInstr
  | F64x2Min_PlainInstr
  | F64x2Max_PlainInstr
  | F64x2Pmin_PlainInstr
  | F64x2Pmax_PlainInstr
  | F64x2RelaxedMin_PlainInstr
  | F64x2RelaxedMax_PlainInstr
  | F64x2RelaxedMadd_PlainInstr
  | F64x2RelaxedNmadd_PlainInstr
  | -- extension vectors (page 26)
    I16x8ExtendLowI8x16S_PlainInstr
  | I16x8ExtendLowI8x16U_PlainInstr
  | I16x8ExtendHighI8x16S_PlainInstr
  | I16x8ExtendHighI8x16U_PlainInstr
  | I32x4ExtendLowI16x8S_PlainInstr
  | I32x4ExtendLowI16x8U_PlainInstr
  | I32x4ExtendHighI16x8S_PlainInstr
  | I32x4ExtendHighI16x8U_PlainInstr
  | I32x4TruncSatF32x4S_PlainInstr
  | I32x4TruncSatF32x4U_PlainInstr
  | I32x4TruncSatF64x2SZero_PlainInstr
  | I32x4TruncSatF64x2UZero_PlainInstr
  | I32x4RelaxedTruncF32x4S_PlainInstr
  | I32x4RelaxedTruncF32x4U_PlainInstr
  | I32x4RelaxedTruncF64x2SZero_PlainInstr
  | I32x4RelaxedTruncF64x2UZero_PlainInstr
  | I64x2ExtendLowI32x4S_PlainInstr
  | I64x2ExtendLowI32x4U_PlainInstr
  | I64x2ExtendHighI32x4S_PlainInstr
  | I64x2ExtendHighI32x4U_PlainInstr
  | F32x4DemoteF64x2Zero_PlainInstr
  | F32x4ConvertI32x4S_PlainInstr
  | F32x4ConvertI32x4U_PlainInstr
  | F64x2PromoteLowF32x4_PlainInstr
  | F64x2ConvertLowI32x4S_PlainInstr
  | F64x2ConvertLowI32x4U_PlainInstr
  | -- pairwise and dot products (page 26)
    I16x8ExtaddPairwiseI8x16S_PlainInstr
  | I16x8ExtaddPairwiseI8x16U_PlainInstr
  | I16x8ExtmulLowI8x16S_PlainInstr
  | I16x8ExtmulLowI8x16U_PlainInstr
  | I16x8ExtmulHighI8x16S_PlainInstr
  | I16x8ExtmulHighI8x16U_PlainInstr
  | I16x8RelaxedDotI8x16I7x16S_PlainInstr
  | I32x4ExtaddPairwiseI16x8S_PlainInstr
  | I32x4ExtaddPairwiseI16x8U_PlainInstr
  | I32x4ExtmulLowI16x8S_PlainInstr
  | I32x4ExtmulLowI16x8U_PlainInstr
  | I32x4ExtmulHighI16x8S_PlainInstr
  | I32x4ExtmulHighI16x8U_PlainInstr
  | I32x4DotI16x8S_PlainInstr
  | I32x4RelaxedDotI8x16I7x16AddS_PlainInstr
  | I64x2ExtmulLowI32x4S_PlainInstr
  | I64x2ExtmulLowI32x4U_PlainInstr
  | I64x2ExtmulHighI32x4S_PlainInstr
  | I64x2ExtmulHighI32x4U_PlainInstr
  deriving (Generic, Eq, Show, Ord)

encMem :: Doc ann -> MemIdx -> MemArg -> Doc ann
encMem op m arg =
  if isMemArgEmpty arg
    then op <+> encodeWat m
    else op <+> encodeWat m <+> encodeWat arg

encMemLane :: Doc ann -> MemIdx -> MemArg -> LaneIdx -> Doc ann
encMemLane op m arg lane =
  if isMemArgEmpty arg
    then op <+> encodeWat m <+> encodeWat lane
    else op <+> encodeWat m <+> encodeWat arg <+> encodeWat lane

instance EncodeWat PlainInstr where
  -- ### 6.5.2 Parametric Instructions
  encodeWat Unreachable_PlainInstr = "unreachable"
  encodeWat Nop_PlainInstr = "nop"
  encodeWat Drop_PlainInstr = "drop"
  encodeWat (Select_PlainInstr rsM) = parens $ maybe "select" (\rs -> "select" <+> hsep (fmap encodeWat rs)) rsM
  -- ### 6.5.3 Plain Control Instructions
  encodeWat (Br_PlainInstr l) = parens $ "br" <+> encodeWat l
  encodeWat (BrIf_PlainInstr l) = parens $ "br_if" <+> encodeWat l
  encodeWat (BrTable_PlainInstr ls l) = parens $ "br_table" <+> hsep (fmap encodeWat ls) <+> encodeWat l
  encodeWat Return_PlainInstr = "return"
  encodeWat (Call_PlainInstr f) = parens $ "call" <+> encodeWat f
  encodeWat (ReturnCall_PlainInstr f) = parens $ "return_call" <+> encodeWat f
  encodeWat (CallIndirect_PlainInstr t tu) = parens $ "call_indirect" <+> encodeWat t <+> encodeWat tu
  encodeWat (ReturnCallIndirect_PlainInstr t tu) = parens $ "return_call_indirect" <+> encodeWat t <+> encodeWat tu
  -- ### 6.5.4 Variable Instructions
  encodeWat (LocalGet_PlainInstr l) = parens $ "local.get" <+> encodeWat l
  encodeWat (LocalSet_PlainInstr l) = parens $ "local.set" <+> encodeWat l
  encodeWat (LocalTee_PlainInstr l) = parens $ "local.tee" <+> encodeWat l
  encodeWat (GlobalGet_PlainInstr g) = parens $ "global.get" <+> encodeWat g
  encodeWat (GlobalSet_PlainInstr g) = parens $ "global.set" <+> encodeWat g
  -- ### 6.5.5 Table Instructions
  encodeWat (TableGet_PlainInstr t) = parens $ "table.get" <+> encodeWat t
  encodeWat (TableSet_PlainInstr t) = parens $ "table.set" <+> encodeWat t
  encodeWat (TableSize_PlainInstr t) = parens $ "table.size" <+> encodeWat t
  encodeWat (TableGrow_PlainInstr t) = parens $ "table.grow" <+> encodeWat t
  encodeWat (TableFill_PlainInstr t) = parens $ "table.fill" <+> encodeWat t
  encodeWat (TableCopy_PlainInstr t1 t2) = parens $ "table.copy" <+> encodeWat t1 <+> encodeWat t2
  encodeWat (TableInit_PlainInstr t e) = parens $ "table.init" <+> encodeWat t <+> encodeWat e
  encodeWat (ElemDrop_PlainInstr e) = parens $ "elem.drop" <+> encodeWat e
  -- ### 6.5.6 Memory Instructions
  encodeWat (I32Load_PlainInstr m arg) = parens $ encMem "i32.load" m arg
  encodeWat (I64Load_PlainInstr m arg) = parens $ encMem "i64.load" m arg
  encodeWat (F32Load_PlainInstr m arg) = parens $ encMem "f32.load" m arg
  encodeWat (F64Load_PlainInstr m arg) = parens $ encMem "f64.load" m arg
  encodeWat (I32Load8S_PlainInstr m arg) = parens $ encMem "i32.load8_s" m arg
  encodeWat (I32Load8U_PlainInstr m arg) = parens $ encMem "i32.load8_u" m arg
  encodeWat (I32Load16S_PlainInstr m arg) = parens $ encMem "i32.load16_s" m arg
  encodeWat (I32Load16U_PlainInstr m arg) = parens $ encMem "i32.load16_u" m arg
  encodeWat (I64Load8S_PlainInstr m arg) = parens $ encMem "i64.load8_s" m arg
  encodeWat (I64Load8U_PlainInstr m arg) = parens $ encMem "i64.load8_u" m arg
  encodeWat (I64Load16S_PlainInstr m arg) = parens $ encMem "i64.load16_s" m arg
  encodeWat (I64Load16U_PlainInstr m arg) = parens $ encMem "i64.load16_u" m arg
  encodeWat (I64Load32S_PlainInstr m arg) = parens $ encMem "i64.load32_s" m arg
  encodeWat (I64Load32U_PlainInstr m arg) = parens $ encMem "i64.load32_u" m arg
  encodeWat (V128Load_PlainInstr m arg) = parens $ encMem "v128.load" m arg
  encodeWat (V128Load8x8S_PlainInstr m arg) = parens $ encMem "v128.load8x8_s" m arg
  encodeWat (V128Load8x8U_PlainInstr m arg) = parens $ encMem "v128.load8x8_u" m arg
  encodeWat (V128Load16x4S_PlainInstr m arg) = parens $ encMem "v128.load16x4_s" m arg
  encodeWat (V128Load16x4U_PlainInstr m arg) = parens $ encMem "v128.load16x4_u" m arg
  encodeWat (V128Load32x2S_PlainInstr m arg) = parens $ encMem "v128.load32x2_s" m arg
  encodeWat (V128Load32x2U_PlainInstr m arg) = parens $ encMem "v128.load32x2_u" m arg
  encodeWat (V128Load8Splat_PlainInstr m arg) = parens $ encMem "v128.load8_splat" m arg
  encodeWat (V128Load16Splat_PlainInstr m arg) = parens $ encMem "v128.load16_splat" m arg
  encodeWat (V128Load32Splat_PlainInstr m arg) = parens $ encMem "v128.load32_splat" m arg
  encodeWat (V128Load64Splat_PlainInstr m arg) = parens $ encMem "v128.load64_splat" m arg
  encodeWat (V128Load32Zero_PlainInstr m arg) = parens $ encMem "v128.load32_zero" m arg
  encodeWat (V128Load64Zero_PlainInstr m arg) = parens $ encMem "v128.load64_zero" m arg
  encodeWat (V128Load8Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.load8_lane" m arg lane
  encodeWat (V128Load16Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.load16_lane" m arg lane
  encodeWat (V128Load32Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.load32_lane" m arg lane
  encodeWat (V128Load64Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.load64_lane" m arg lane
  encodeWat (I32Store_PlainInstr m arg) = parens $ encMem "i32.store" m arg
  encodeWat (I64Store_PlainInstr m arg) = parens $ encMem "i64.store" m arg
  encodeWat (F32Store_PlainInstr m arg) = parens $ encMem "f32.store" m arg
  encodeWat (F64Store_PlainInstr m arg) = parens $ encMem "f64.store" m arg
  encodeWat (I32Store8_PlainInstr m arg) = parens $ encMem "i32.store8" m arg
  encodeWat (I32Store16_PlainInstr m arg) = parens $ encMem "i32.store16" m arg
  encodeWat (I64Store8_PlainInstr m arg) = parens $ encMem "i64.store8" m arg
  encodeWat (I64Store16_PlainInstr m arg) = parens $ encMem "i64.store16" m arg
  encodeWat (I64Store32_PlainInstr m arg) = parens $ encMem "i64.store32" m arg
  encodeWat (V128Store_PlainInstr m arg) = parens $ encMem "v128.store" m arg
  encodeWat (V128Store8Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.store8_lane" m arg lane
  encodeWat (V128Store16Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.store16_lane" m arg lane
  encodeWat (V128Store32Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.store32_lane" m arg lane
  encodeWat (V128Store64Lane_PlainInstr m arg lane) = parens $ encMemLane "v128.store64_lane" m arg lane
  encodeWat (MemorySize_PlainInstr m) = parens $ "memory.size" <+> encodeWat m
  encodeWat (MemoryGrow_PlainInstr m) = parens $ "memory.grow" <+> encodeWat m
  encodeWat (MemoryFill_PlainInstr m) = parens $ "memory.fill" <+> encodeWat m
  encodeWat (MemoryCopy_PlainInstr m1 m2) = parens $ "memory.copy" <+> encodeWat m1 <+> encodeWat m2
  encodeWat (MemoryInit_PlainInstr m d) = parens $ "memory.init" <+> encodeWat m <+> encodeWat d
  encodeWat (DataDrop_PlainInstr d) = parens $ "data.drop" <+> encodeWat d
  -- ### 6.5.7 Reference Instructions
  encodeWat (RefNull_PlainInstr ht) = parens $ "ref.null" <+> encodeWat ht
  encodeWat (RefFunc_PlainInstr f) = parens $ "ref.func" <+> encodeWat f
  encodeWat RefIsNull_PlainInstr = parens "ref.is_null"
  encodeWat RefAsNonNull_PlainInstr = parens "ref.as_non_null"
  encodeWat RefEq_PlainInstr = parens "ref.eq"
  encodeWat (RefTest_PlainInstr rt) = parens $ "ref.test" <+> encodeWat rt
  encodeWat (RefCast_PlainInstr rt) = parens $ "ref.cast" <+> encodeWat rt
  -- ### 6.5.8 Aggregate Instructions
  encodeWat RefI31_PlainInstr = parens "ref.i31"
  encodeWat I31GetS_PlainInstr = parens "i31.get_s"
  encodeWat I31GetU_PlainInstr = parens "i31.get_u"
  encodeWat (StructNew_PlainInstr t) = parens $ "struct.new" <+> encodeWat t
  encodeWat (StructNewDefault_PlainInstr t) = parens $ "struct.new_default" <+> encodeWat t
  encodeWat (StructGet_PlainInstr t f) = parens $ "struct.get" <+> encodeWat t <+> encodeWat f
  encodeWat (StructGetS_PlainInstr t f) = parens $ "struct.get_s" <+> encodeWat t <+> encodeWat f
  encodeWat (StructGetU_PlainInstr t f) = parens $ "struct.get_u" <+> encodeWat t <+> encodeWat f
  encodeWat (StructSet_PlainInstr t f) = parens $ "struct.set" <+> encodeWat t <+> encodeWat f
  encodeWat (ArrayNew_PlainInstr t) = parens $ "array.new" <+> encodeWat t
  encodeWat (ArrayNewDefault_PlainInstr t) = parens $ "array.new_default" <+> encodeWat t
  encodeWat (ArrayNewFixed_PlainInstr t n) = parens $ "array.new_fixed" <+> encodeWat t <+> pretty n
  encodeWat (ArrayNewData_PlainInstr t d) = parens $ "array.new_data" <+> encodeWat t <+> encodeWat d
  encodeWat (ArrayNewElem_PlainInstr t e) = parens $ "array.new_elem" <+> encodeWat t <+> encodeWat e
  encodeWat (ArrayGet_PlainInstr t) = parens $ "array.get" <+> encodeWat t
  encodeWat (ArrayGetS_PlainInstr t) = parens $ "array.get_s" <+> encodeWat t
  encodeWat (ArrayGetU_PlainInstr t) = parens $ "array.get_u" <+> encodeWat t
  encodeWat (ArraySet_PlainInstr t) = parens $ "array.set" <+> encodeWat t
  encodeWat ArrayLen_PlainInstr = parens "array.len"
  encodeWat (ArrayFill_PlainInstr t) = parens $ "array.fill" <+> encodeWat t
  encodeWat (ArrayCopy_PlainInstr t1 t2) = parens $ "array.copy" <+> encodeWat t1 <+> encodeWat t2
  encodeWat (ArrayInitData_PlainInstr t d) = parens $ "array.init_data" <+> encodeWat t <+> encodeWat d
  encodeWat (ArrayInitElem_PlainInstr t e) = parens $ "array.init_elem" <+> encodeWat t <+> encodeWat e
  encodeWat AnyConvertExtern_PlainInstr = parens "any.convert_extern"
  encodeWat ExternConvertAny_PlainInstr = parens "extern.convert_any"
  -- ### 6.5.9 Numeric Const Instructions
  encodeWat (I32Const_PlainInstr c) = parens $ "i32.const" <+> pretty c
  encodeWat (I64Const_PlainInstr c) = parens $ "i64.const" <+> pretty c
  encodeWat (F32Const_PlainInstr c) = parens $ "f32.const" <+> pretty c
  encodeWat (F64Const_PlainInstr c) = parens $ "f64.const" <+> pretty c
  -- ### 6.5.9 Numeric Operators
  -- i32 operators
  encodeWat I32Eqz_PlainInstr = "i32.eqz"
  encodeWat I32Eq_PlainInstr = "i32.eq"
  encodeWat I32Ne_PlainInstr = "i32.ne"
  encodeWat I32LtS_PlainInstr = "i32.lt_s"
  encodeWat I32LtU_PlainInstr = "i32.lt_u"
  encodeWat I32GtS_PlainInstr = "i32.gt_s"
  encodeWat I32GtU_PlainInstr = "i32.gt_u"
  encodeWat I32LeS_PlainInstr = "i32.le_s"
  encodeWat I32LeU_PlainInstr = "i32.le_u"
  encodeWat I32GeS_PlainInstr = "i32.ge_s"
  encodeWat I32GeU_PlainInstr = "i32.ge_u"
  encodeWat I32Clz_PlainInstr = "i32.clz"
  encodeWat I32Ctz_PlainInstr = "i32.ctz"
  encodeWat I32Popcnt_PlainInstr = "i32.popcnt"
  encodeWat I32Extend8S_PlainInstr = "i32.extend8_s"
  encodeWat I32Extend16S_PlainInstr = "i32.extend16_s"
  encodeWat I32Add_PlainInstr = "i32.add"
  encodeWat I32Sub_PlainInstr = "i32.sub"
  encodeWat I32Mul_PlainInstr = "i32.mul"
  encodeWat I32DivS_PlainInstr = "i32.div_s"
  encodeWat I32DivU_PlainInstr = "i32.div_u"
  encodeWat I32RemS_PlainInstr = "i32.rem_s"
  encodeWat I32RemU_PlainInstr = "i32.rem_u"
  encodeWat I32And_PlainInstr = "i32.and"
  encodeWat I32Or_PlainInstr = "i32.or"
  encodeWat I32Xor_PlainInstr = "i32.xor"
  encodeWat I32Shl_PlainInstr = "i32.shl"
  encodeWat I32ShrS_PlainInstr = "i32.shr_s"
  encodeWat I32ShrU_PlainInstr = "i32.shr_u"
  encodeWat I32Rotl_PlainInstr = "i32.rotl"
  encodeWat I32Rotr_PlainInstr = "i32.rotr"
  -- i64 operators
  encodeWat I64Eqz_PlainInstr = "i64.eqz"
  encodeWat I64Eq_PlainInstr = "i64.eq"
  encodeWat I64Ne_PlainInstr = "i64.ne"
  encodeWat I64LtS_PlainInstr = "i64.lt_s"
  encodeWat I64LtU_PlainInstr = "i64.lt_u"
  encodeWat I64GtS_PlainInstr = "i64.gt_s"
  encodeWat I64GtU_PlainInstr = "i64.gt_u"
  encodeWat I64LeS_PlainInstr = "i64.le_s"
  encodeWat I64LeU_PlainInstr = "i64.le_u"
  encodeWat I64GeS_PlainInstr = "i64.ge_s"
  encodeWat I64GeU_PlainInstr = "i64.ge_u"
  encodeWat I64Clz_PlainInstr = "i64.clz"
  encodeWat I64Ctz_PlainInstr = "i64.ctz"
  encodeWat I64Popcnt_PlainInstr = "i64.popcnt"
  encodeWat I64Extend8S_PlainInstr = "i64.extend8_s"
  encodeWat I64Extend16S_PlainInstr = "i64.extend16_s"
  encodeWat I64Extend32S_PlainInstr = "i64.extend32_s"
  encodeWat I64Add_PlainInstr = "i64.add"
  encodeWat I64Sub_PlainInstr = "i64.sub"
  encodeWat I64Mul_PlainInstr = "i64.mul"
  encodeWat I64DivS_PlainInstr = "i64.div_s"
  encodeWat I64DivU_PlainInstr = "i64.div_u"
  encodeWat I64RemS_PlainInstr = "i64.rem_s"
  encodeWat I64RemU_PlainInstr = "i64.rem_u"
  encodeWat I64And_PlainInstr = "i64.and"
  encodeWat I64Or_PlainInstr = "i64.or"
  encodeWat I64Xor_PlainInstr = "i64.xor"
  encodeWat I64Shl_PlainInstr = "i64.shl"
  encodeWat I64ShrS_PlainInstr = "i64.shr_s"
  encodeWat I64ShrU_PlainInstr = "i64.shr_u"
  encodeWat I64Rotl_PlainInstr = "i64.rotl"
  encodeWat I64Rotr_PlainInstr = "i64.rotr"
  -- f32 operators
  encodeWat F32Eq_PlainInstr = "f32.eq"
  encodeWat F32Ne_PlainInstr = "f32.ne"
  encodeWat F32Lt_PlainInstr = "f32.lt"
  encodeWat F32Gt_PlainInstr = "f32.gt"
  encodeWat F32Le_PlainInstr = "f32.le"
  encodeWat F32Ge_PlainInstr = "f32.ge"
  encodeWat F32Abs_PlainInstr = "f32.abs"
  encodeWat F32Neg_PlainInstr = "f32.neg"
  encodeWat F32Sqrt_PlainInstr = "f32.sqrt"
  encodeWat F32Ceil_PlainInstr = "f32.ceil"
  encodeWat F32Floor_PlainInstr = "f32.floor"
  encodeWat F32Trunc_PlainInstr = "f32.trunc"
  encodeWat F32Nearest_PlainInstr = "f32.nearest"
  encodeWat F32Add_PlainInstr = "f32.add"
  encodeWat F32Sub_PlainInstr = "f32.sub"
  encodeWat F32Mul_PlainInstr = "f32.mul"
  encodeWat F32Div_PlainInstr = "f32.div"
  encodeWat F32Min_PlainInstr = "f32.min"
  encodeWat F32Max_PlainInstr = "f32.max"
  encodeWat F32Copysign_PlainInstr = "f32.copysign"
  -- f64 operators
  encodeWat F64Eq_PlainInstr = "f64.eq"
  encodeWat F64Ne_PlainInstr = "f64.ne"
  encodeWat F64Lt_PlainInstr = "f64.lt"
  encodeWat F64Gt_PlainInstr = "f64.gt"
  encodeWat F64Le_PlainInstr = "f64.le"
  encodeWat F64Ge_PlainInstr = "f64.ge"
  encodeWat F64Abs_PlainInstr = "f64.abs"
  encodeWat F64Neg_PlainInstr = "f64.neg"
  encodeWat F64Sqrt_PlainInstr = "f64.sqrt"
  encodeWat F64Ceil_PlainInstr = "f64.ceil"
  encodeWat F64Floor_PlainInstr = "f64.floor"
  encodeWat F64Trunc_PlainInstr = "f64.trunc"
  encodeWat F64Nearest_PlainInstr = "f64.nearest"
  encodeWat F64Add_PlainInstr = "f64.add"
  encodeWat F64Sub_PlainInstr = "f64.sub"
  encodeWat F64Mul_PlainInstr = "f64.mul"
  encodeWat F64Div_PlainInstr = "f64.div"
  encodeWat F64Min_PlainInstr = "f64.min"
  encodeWat F64Max_PlainInstr = "f64.max"
  encodeWat F64Copysign_PlainInstr = "f64.copysign"
  -- conversions
  encodeWat I32WrapI64_PlainInstr = "i32.wrap_i64"
  encodeWat I32TruncF32S_PlainInstr = "i32.trunc_f32_s"
  encodeWat I32TruncF32U_PlainInstr = "i32.trunc_f32_u"
  encodeWat I32TruncF64S_PlainInstr = "i32.trunc_f64_s"
  encodeWat I32TruncF64U_PlainInstr = "i32.trunc_f64_u"
  encodeWat I32TruncSatF32S_PlainInstr = "i32.trunc_sat_f32_s"
  encodeWat I32TruncSatF32U_PlainInstr = "i32.trunc_sat_f32_u"
  encodeWat I32TruncSatF64S_PlainInstr = "i32.trunc_sat_f64_s"
  encodeWat I32TruncSatF64U_PlainInstr = "i32.trunc_sat_f64_u"
  encodeWat I64ExtendI32S_PlainInstr = "i64.extend_i32_s"
  encodeWat I64ExtendI32U_PlainInstr = "i64.extend_i32_u"
  encodeWat I64TruncF32S_PlainInstr = "i64.trunc_f32_s"
  encodeWat I64TruncF32U_PlainInstr = "i64.trunc_f32_u"
  encodeWat I64TruncF64S_PlainInstr = "i64.trunc_f64_s"
  encodeWat I64TruncF64U_PlainInstr = "i64.trunc_f64_u"
  encodeWat I64TruncSatF32S_PlainInstr = "i64.trunc_sat_f32_s"
  encodeWat I64TruncSatF32U_PlainInstr = "i64.trunc_sat_f32_u"
  encodeWat I64TruncSatF64S_PlainInstr = "i64.trunc_sat_f64_s"
  encodeWat I64TruncSatF64U_PlainInstr = "i64.trunc_sat_f64_u"
  encodeWat F32DemoteF64_PlainInstr = "f32.demote_f64"
  encodeWat F32ConvertI32S_PlainInstr = "f32.convert_i32_s"
  encodeWat F32ConvertI32U_PlainInstr = "f32.convert_i32_u"
  encodeWat F32ConvertI64S_PlainInstr = "f32.convert_i64_s"
  encodeWat F32ConvertI64U_PlainInstr = "f32.convert_i64_u"
  encodeWat F64PromoteF32_PlainInstr = "f64.promote_f32"
  encodeWat F64ConvertI32S_PlainInstr = "f64.convert_i32_s"
  encodeWat F64ConvertI32U_PlainInstr = "f64.convert_i32_u"
  encodeWat F64ConvertI64S_PlainInstr = "f64.convert_i64_s"
  encodeWat F64ConvertI64U_PlainInstr = "f64.convert_i64_u"
  encodeWat I32ReinterpretF32_PlainInstr = "i32.reinterpret_f32"
  encodeWat I64ReinterpretF64_PlainInstr = "i64.reinterpret_f64"
  encodeWat F32ReinterpretI32_PlainInstr = "f32.reinterpret_i32"
  encodeWat F64ReinterpretI64_PlainInstr = "f64.reinterpret_i64"
  -- ### 6.5.10 Vector Instructions
  -- vector const and shuffle
  encodeWat (V128Const_PlainInstr shape cs) = parens $ "v128.const" <+> reflow shape <+> hsep (fmap pretty cs)
  encodeWat (I8x16Shuffle_PlainInstr lanes) = parens $ "i8x16.shuffle" <+> hsep (fmap encodeWat lanes)
  -- swizzle and splats
  encodeWat I8x16Swizzle_PlainInstr = "i8x16.swizzle"
  encodeWat I8x16RelaxedSwizzle_PlainInstr = "i8x16.relaxed_swizzle"
  encodeWat I8x16Splat_PlainInstr = "i8x16.splat"
  encodeWat I16x8Splat_PlainInstr = "i16x8.splat"
  encodeWat I32x4Splat_PlainInstr = "i32x4.splat"
  encodeWat I64x2Splat_PlainInstr = "i64x2.splat"
  encodeWat F32x4Splat_PlainInstr = "f32x4.splat"
  encodeWat F64x2Splat_PlainInstr = "f64x2.splat"
  -- lanes
  encodeWat (I8x16ExtractLaneS_PlainInstr lane) = parens $ "i8x16.extract_lane_s" <+> encodeWat lane
  encodeWat (I8x16ExtractLaneU_PlainInstr lane) = parens $ "i8x16.extract_lane_u" <+> encodeWat lane
  encodeWat (I16x8ExtractLaneS_PlainInstr lane) = parens $ "i16x8.extract_lane_s" <+> encodeWat lane
  encodeWat (I16x8ExtractLaneU_PlainInstr lane) = parens $ "i16x8.extract_lane_u" <+> encodeWat lane
  encodeWat (I32x4ExtractLane_PlainInstr lane) = parens $ "i32x4.extract_lane" <+> encodeWat lane
  encodeWat (I64x2ExtractLane_PlainInstr lane) = parens $ "i64x2.extract_lane" <+> encodeWat lane
  encodeWat (F32x4ExtractLane_PlainInstr lane) = parens $ "f32x4.extract_lane" <+> encodeWat lane
  encodeWat (F64x2ExtractLane_PlainInstr lane) = parens $ "f64x2.extract_lane" <+> encodeWat lane
  encodeWat (I8x16ReplaceLane_PlainInstr lane) = parens $ "i8x16.replace_lane" <+> encodeWat lane
  encodeWat (I16x8ReplaceLane_PlainInstr lane) = parens $ "i16x8.replace_lane" <+> encodeWat lane
  encodeWat (I32x4ReplaceLane_PlainInstr lane) = parens $ "i32x4.replace_lane" <+> encodeWat lane
  encodeWat (I64x2ReplaceLane_PlainInstr lane) = parens $ "i64x2.replace_lane" <+> encodeWat lane
  encodeWat (F32x4ReplaceLane_PlainInstr lane) = parens $ "f32x4.replace_lane" <+> encodeWat lane
  encodeWat (F64x2ReplaceLane_PlainInstr lane) = parens $ "f64x2.replace_lane" <+> encodeWat lane
  -- vector ops
  encodeWat V128AnyTrue_PlainInstr = "v128.any_true"
  encodeWat V128Not_PlainInstr = "v128.not"
  encodeWat V128And_PlainInstr = "v128.and"
  encodeWat V128Andnot_PlainInstr = "v128.andnot"
  encodeWat V128Or_PlainInstr = "v128.or"
  encodeWat V128Xor_PlainInstr = "v128.xor"
  encodeWat V128Bitselect_PlainInstr = "v128.bitselect"
  encodeWat I8x16AllTrue_PlainInstr = "i8x16.all_true"
  encodeWat I8x16Eq_PlainInstr = "i8x16.eq"
  encodeWat I8x16Ne_PlainInstr = "i8x16.ne"
  encodeWat I8x16LtS_PlainInstr = "i8x16.lt_s"
  encodeWat I8x16LtU_PlainInstr = "i8x16.lt_u"
  encodeWat I8x16GtS_PlainInstr = "i8x16.gt_s"
  encodeWat I8x16GtU_PlainInstr = "i8x16.gt_u"
  encodeWat I8x16LeS_PlainInstr = "i8x16.le_s"
  encodeWat I8x16LeU_PlainInstr = "i8x16.le_u"
  encodeWat I8x16GeS_PlainInstr = "i8x16.ge_s"
  encodeWat I8x16GeU_PlainInstr = "i8x16.ge_u"
  encodeWat I8x16Abs_PlainInstr = "i8x16.abs"
  encodeWat I8x16Neg_PlainInstr = "i8x16.neg"
  encodeWat I8x16Popcnt_PlainInstr = "i8x16.popcnt"
  encodeWat I8x16Add_PlainInstr = "i8x16.add"
  encodeWat I8x16AddSatS_PlainInstr = "i8x16.add_sat_s"
  encodeWat I8x16AddSatU_PlainInstr = "i8x16.add_sat_u"
  encodeWat I8x16Sub_PlainInstr = "i8x16.sub"
  encodeWat I8x16SubSatS_PlainInstr = "i8x16.sub_sat_s"
  encodeWat I8x16SubSatU_PlainInstr = "i8x16.sub_sat_u"
  encodeWat I8x16MinS_PlainInstr = "i8x16.min_s"
  encodeWat I8x16MinU_PlainInstr = "i8x16.min_u"
  encodeWat I8x16MaxS_PlainInstr = "i8x16.max_s"
  encodeWat I8x16MaxU_PlainInstr = "i8x16.max_u"
  encodeWat I8x16AvgrU_PlainInstr = "i8x16.avgr_u"
  encodeWat I8x16RelaxedLaneselect_PlainInstr = "i8x16.relaxed_laneselect"
  encodeWat I8x16Shl_PlainInstr = "i8x16.shl"
  encodeWat I8x16ShrS_PlainInstr = "i8x16.shr_s"
  encodeWat I8x16ShrU_PlainInstr = "i8x16.shr_u"
  encodeWat I8x16Bitmask_PlainInstr = "i8x16.bitmask"
  encodeWat I8x16NarrowI16x8S_PlainInstr = "i8x16.narrow_i16x8_s"
  encodeWat I8x16NarrowI16x8U_PlainInstr = "i8x16.narrow_i16x8_u"
  -- i16x8 ops
  encodeWat I16x8AllTrue_PlainInstr = "i16x8.all_true"
  encodeWat I16x8Eq_PlainInstr = "i16x8.eq"
  encodeWat I16x8Ne_PlainInstr = "i16x8.ne"
  encodeWat I16x8LtS_PlainInstr = "i16x8.lt_s"
  encodeWat I16x8LtU_PlainInstr = "i16x8.lt_u"
  encodeWat I16x8GtS_PlainInstr = "i16x8.gt_s"
  encodeWat I16x8GtU_PlainInstr = "i16x8.gt_u"
  encodeWat I16x8LeS_PlainInstr = "i16x8.le_s"
  encodeWat I16x8LeU_PlainInstr = "i16x8.le_u"
  encodeWat I16x8GeS_PlainInstr = "i16x8.ge_s"
  encodeWat I16x8GeU_PlainInstr = "i16x8.ge_u"
  encodeWat I16x8Abs_PlainInstr = "i16x8.abs"
  encodeWat I16x8Neg_PlainInstr = "i16x8.neg"
  encodeWat I16x8Add_PlainInstr = "i16x8.add"
  encodeWat I16x8AddSatS_PlainInstr = "i16x8.add_sat_s"
  encodeWat I16x8AddSatU_PlainInstr = "i16x8.add_sat_u"
  encodeWat I16x8Sub_PlainInstr = "i16x8.sub"
  encodeWat I16x8SubSatS_PlainInstr = "i16x8.sub_sat_s"
  encodeWat I16x8SubSatU_PlainInstr = "i16x8.sub_sat_u"
  encodeWat I16x8Mul_PlainInstr = "i16x8.mul"
  encodeWat I16x8MinS_PlainInstr = "i16x8.min_s"
  encodeWat I16x8MinU_PlainInstr = "i16x8.min_u"
  encodeWat I16x8MaxS_PlainInstr = "i16x8.max_s"
  encodeWat I16x8MaxU_PlainInstr = "i16x8.max_u"
  encodeWat I16x8AvgrU_PlainInstr = "i16x8.avgr_u"
  encodeWat I16x8Q15mulrSatS_PlainInstr = "i16x8.q15mulr_sat_s"
  encodeWat I16x8RelaxedQ15mulrS_PlainInstr = "i16x8.relaxed_q15mulr_s"
  encodeWat I16x8RelaxedLaneselect_PlainInstr = "i16x8.relaxed_laneselect"
  encodeWat I16x8Shl_PlainInstr = "i16x8.shl"
  encodeWat I16x8ShrS_PlainInstr = "i16x8.shr_s"
  encodeWat I16x8ShrU_PlainInstr = "i16x8.shr_u"
  encodeWat I16x8Bitmask_PlainInstr = "i16x8.bitmask"
  encodeWat I16x8NarrowI32x4S_PlainInstr = "i16x8.narrow_i32x4_s"
  encodeWat I16x8NarrowI32x4U_PlainInstr = "i16x8.narrow_i32x4_u"
  -- i32x4 ops
  encodeWat I32x4AllTrue_PlainInstr = "i32x4.all_true"
  encodeWat I32x4Eq_PlainInstr = "i32x4.eq"
  encodeWat I32x4Ne_PlainInstr = "i32x4.ne"
  encodeWat I32x4LtS_PlainInstr = "i32x4.lt_s"
  encodeWat I32x4LtU_PlainInstr = "i32x4.lt_u"
  encodeWat I32x4GtS_PlainInstr = "i32x4.gt_s"
  encodeWat I32x4GtU_PlainInstr = "i32x4.gt_u"
  encodeWat I32x4LeS_PlainInstr = "i32x4.le_s"
  encodeWat I32x4LeU_PlainInstr = "i32x4.le_u"
  encodeWat I32x4GeS_PlainInstr = "i32x4.ge_s"
  encodeWat I32x4GeU_PlainInstr = "i32x4.ge_u"
  encodeWat I32x4Abs_PlainInstr = "i32x4.abs"
  encodeWat I32x4Neg_PlainInstr = "i32x4.neg"
  encodeWat I32x4Add_PlainInstr = "i32x4.add"
  encodeWat I32x4Sub_PlainInstr = "i32x4.sub"
  encodeWat I32x4Mul_PlainInstr = "i32x4.mul"
  encodeWat I32x4MinS_PlainInstr = "i32x4.min_s"
  encodeWat I32x4MinU_PlainInstr = "i32x4.min_u"
  encodeWat I32x4MaxS_PlainInstr = "i32x4.max_s"
  encodeWat I32x4MaxU_PlainInstr = "i32x4.max_u"
  encodeWat I32x4RelaxedLaneselect_PlainInstr = "i32x4.relaxed_laneselect"
  encodeWat I32x4Shl_PlainInstr = "i32x4.shl"
  encodeWat I32x4ShrS_PlainInstr = "i32x4.shr_s"
  encodeWat I32x4ShrU_PlainInstr = "i32x4.shr_u"
  encodeWat I32x4Bitmask_PlainInstr = "i32x4.bitmask"
  -- i64x2 ops
  encodeWat I64x2AllTrue_PlainInstr = "i64x2.all_true"
  encodeWat I64x2Eq_PlainInstr = "i64x2.eq"
  encodeWat I64x2Ne_PlainInstr = "i64x2.ne"
  encodeWat I64x2LtS_PlainInstr = "i64x2.lt_s"
  encodeWat I64x2GtS_PlainInstr = "i64x2.gt_s"
  encodeWat I64x2LeS_PlainInstr = "i64x2.le_s"
  encodeWat I64x2GeS_PlainInstr = "i64x2.ge_s"
  encodeWat I64x2Abs_PlainInstr = "i64x2.abs"
  encodeWat I64x2Neg_PlainInstr = "i64x2.neg"
  encodeWat I64x2Add_PlainInstr = "i64x2.add"
  encodeWat I64x2Sub_PlainInstr = "i64x2.sub"
  encodeWat I64x2Mul_PlainInstr = "i64x2.mul"
  encodeWat I64x2RelaxedLaneselect_PlainInstr = "i64x2.relaxed_laneselect"
  encodeWat I64x2Shl_PlainInstr = "i64x2.shl"
  encodeWat I64x2ShrS_PlainInstr = "i64x2.shr_s"
  encodeWat I64x2ShrU_PlainInstr = "i64x2.shr_u"
  encodeWat I64x2Bitmask_PlainInstr = "i64x2.bitmask"
  -- f32x4 ops
  encodeWat F32x4Eq_PlainInstr = "f32x4.eq"
  encodeWat F32x4Ne_PlainInstr = "f32x4.ne"
  encodeWat F32x4Lt_PlainInstr = "f32x4.lt"
  encodeWat F32x4Gt_PlainInstr = "f32x4.gt"
  encodeWat F32x4Le_PlainInstr = "f32x4.le"
  encodeWat F32x4Ge_PlainInstr = "f32x4.ge"
  encodeWat F32x4Abs_PlainInstr = "f32x4.abs"
  encodeWat F32x4Neg_PlainInstr = "f32x4.neg"
  encodeWat F32x4Sqrt_PlainInstr = "f32x4.sqrt"
  encodeWat F32x4Ceil_PlainInstr = "f32x4.ceil"
  encodeWat F32x4Floor_PlainInstr = "f32x4.floor"
  encodeWat F32x4Trunc_PlainInstr = "f32x4.trunc"
  encodeWat F32x4Nearest_PlainInstr = "f32x4.nearest"
  encodeWat F32x4Add_PlainInstr = "f32x4.add"
  encodeWat F32x4Sub_PlainInstr = "f32x4.sub"
  encodeWat F32x4Mul_PlainInstr = "f32x4.mul"
  encodeWat F32x4Div_PlainInstr = "f32x4.div"
  encodeWat F32x4Min_PlainInstr = "f32x4.min"
  encodeWat F32x4Max_PlainInstr = "f32x4.max"
  encodeWat F32x4Pmin_PlainInstr = "f32x4.pmin"
  encodeWat F32x4Pmax_PlainInstr = "f32x4.pmax"
  encodeWat F32x4RelaxedMin_PlainInstr = "f32x4.relaxed_min"
  encodeWat F32x4RelaxedMax_PlainInstr = "f32x4.relaxed_max"
  encodeWat F32x4RelaxedMadd_PlainInstr = "f32x4.relaxed_madd"
  encodeWat F32x4RelaxedNmadd_PlainInstr = "f32x4.relaxed_nmadd"
  -- f64x2 ops
  encodeWat F64x2Eq_PlainInstr = "f64x2.eq"
  encodeWat F64x2Ne_PlainInstr = "f64x2.ne"
  encodeWat F64x2Lt_PlainInstr = "f64x2.lt"
  encodeWat F64x2Gt_PlainInstr = "f64x2.gt"
  encodeWat F64x2Le_PlainInstr = "f64x2.le"
  encodeWat F64x2Ge_PlainInstr = "f64x2.ge"
  encodeWat F64x2Abs_PlainInstr = "f64x2.abs"
  encodeWat F64x2Neg_PlainInstr = "f64x2.neg"
  encodeWat F64x2Sqrt_PlainInstr = "f64x2.sqrt"
  encodeWat F64x2Ceil_PlainInstr = "f64x2.ceil"
  encodeWat F64x2Floor_PlainInstr = "f64x2.floor"
  encodeWat F64x2Trunc_PlainInstr = "f64x2.trunc"
  encodeWat F64x2Nearest_PlainInstr = "f64x2.nearest"
  encodeWat F64x2Add_PlainInstr = "f64x2.add"
  encodeWat F64x2Sub_PlainInstr = "f64x2.sub"
  encodeWat F64x2Mul_PlainInstr = "f64x2.mul"
  encodeWat F64x2Div_PlainInstr = "f64x2.div"
  encodeWat F64x2Min_PlainInstr = "f64x2.min"
  encodeWat F64x2Max_PlainInstr = "f64x2.max"
  encodeWat F64x2Pmin_PlainInstr = "f64x2.pmin"
  encodeWat F64x2Pmax_PlainInstr = "f64x2.pmax"
  encodeWat F64x2RelaxedMin_PlainInstr = "f64x2.relaxed_min"
  encodeWat F64x2RelaxedMax_PlainInstr = "f64x2.relaxed_max"
  encodeWat F64x2RelaxedMadd_PlainInstr = "f64x2.relaxed_madd"
  encodeWat F64x2RelaxedNmadd_PlainInstr = "f64x2.relaxed_nmadd"
  -- extension vectors (page 26)
  encodeWat I16x8ExtendLowI8x16S_PlainInstr = "i16x8.extend_low_i8x16_s"
  encodeWat I16x8ExtendLowI8x16U_PlainInstr = "i16x8.extend_low_i8x16_u"
  encodeWat I16x8ExtendHighI8x16S_PlainInstr = "i16x8.extend_high_i8x16_s"
  encodeWat I16x8ExtendHighI8x16U_PlainInstr = "i16x8.extend_high_i8x16_u"
  encodeWat I32x4ExtendLowI16x8S_PlainInstr = "i32x4.extend_low_i16x8_s"
  encodeWat I32x4ExtendLowI16x8U_PlainInstr = "i32x4.extend_low_i16x8_u"
  encodeWat I32x4ExtendHighI16x8S_PlainInstr = "i32x4.extend_high_i16x8_s"
  encodeWat I32x4ExtendHighI16x8U_PlainInstr = "i32x4.extend_high_i16x8_u"
  encodeWat I32x4TruncSatF32x4S_PlainInstr = "i32x4.trunc_sat_f32x4_s"
  encodeWat I32x4TruncSatF32x4U_PlainInstr = "i32x4.trunc_sat_f32x4_u"
  encodeWat I32x4TruncSatF64x2SZero_PlainInstr = "i32x4.trunc_sat_f64x2_s_zero"
  encodeWat I32x4TruncSatF64x2UZero_PlainInstr = "i32x4.trunc_sat_f64x2_u_zero"
  encodeWat I32x4RelaxedTruncF32x4S_PlainInstr = "i32x4.relaxed_trunc_f32x4_s"
  encodeWat I32x4RelaxedTruncF32x4U_PlainInstr = "i32x4.relaxed_trunc_f32x4_u"
  encodeWat I32x4RelaxedTruncF64x2SZero_PlainInstr = "i32x4.relaxed_trunc_f64x2_s_zero"
  encodeWat I32x4RelaxedTruncF64x2UZero_PlainInstr = "i32x4.relaxed_trunc_f64x2_u_zero"
  encodeWat I64x2ExtendLowI32x4S_PlainInstr = "i64x2.extend_low_i32x4_s"
  encodeWat I64x2ExtendLowI32x4U_PlainInstr = "i64x2.extend_low_i32x4_u"
  encodeWat I64x2ExtendHighI32x4S_PlainInstr = "i64x2.extend_high_i32x4_s"
  encodeWat I64x2ExtendHighI32x4U_PlainInstr = "i64x2.extend_high_i32x4_u"
  encodeWat F32x4DemoteF64x2Zero_PlainInstr = "f32x4.demote_f64x2_zero"
  encodeWat F32x4ConvertI32x4S_PlainInstr = "f32x4.convert_i32x4_s"
  encodeWat F32x4ConvertI32x4U_PlainInstr = "f32x4.convert_i32x4_u"
  encodeWat F64x2PromoteLowF32x4_PlainInstr = "f64x2.promote_low_f32x4"
  encodeWat F64x2ConvertLowI32x4S_PlainInstr = "f64x2.convert_low_i32x4_s"
  encodeWat F64x2ConvertLowI32x4U_PlainInstr = "f64x2.convert_low_i32x4_u"
  -- pairwise and dot products (page 26)
  encodeWat I16x8ExtaddPairwiseI8x16S_PlainInstr = "i16x8.extadd_pairwise_i8x16_s"
  encodeWat I16x8ExtaddPairwiseI8x16U_PlainInstr = "i16x8.extadd_pairwise_i8x16_u"
  encodeWat I16x8ExtmulLowI8x16S_PlainInstr = "i16x8.extmul_low_i8x16_s"
  encodeWat I16x8ExtmulLowI8x16U_PlainInstr = "i16x8.extmul_low_i8x16_u"
  encodeWat I16x8ExtmulHighI8x16S_PlainInstr = "i16x8.extmul_high_i8x16_s"
  encodeWat I16x8ExtmulHighI8x16U_PlainInstr = "i16x8.extmul_high_i8x16_u"
  encodeWat I16x8RelaxedDotI8x16I7x16S_PlainInstr = "i16x8.relaxed_dot_i8x16_i7x16_s"
  encodeWat I32x4ExtaddPairwiseI16x8S_PlainInstr = "i32x4.extadd_pairwise_i16x8_s"
  encodeWat I32x4ExtaddPairwiseI16x8U_PlainInstr = "i32x4.extadd_pairwise_i16x8_u"
  encodeWat I32x4ExtmulLowI16x8S_PlainInstr = "i32x4.extmul_low_i16x8_s"
  encodeWat I32x4ExtmulLowI16x8U_PlainInstr = "i32x4.extmul_low_i16x8_u"
  encodeWat I32x4ExtmulHighI16x8S_PlainInstr = "i32x4.extmul_high_i16x8_s"
  encodeWat I32x4ExtmulHighI16x8U_PlainInstr = "i32x4.extmul_high_i16x8_u"
  encodeWat I32x4DotI16x8S_PlainInstr = "i32x4.dot_i16x8_s"
  encodeWat I32x4RelaxedDotI8x16I7x16AddS_PlainInstr = "i32x4.relaxed_dot_i8x16_i7x16_add_s"
  encodeWat I64x2ExtmulLowI32x4S_PlainInstr = "i64x2.extmul_low_i32x4_s"
  encodeWat I64x2ExtmulLowI32x4U_PlainInstr = "i64x2.extmul_low_i32x4_u"
  encodeWat I64x2ExtmulHighI32x4S_PlainInstr = "i64x2.extmul_high_i32x4_s"
  encodeWat I64x2ExtmulHighI32x4U_PlainInstr = "i64x2.extmul_high_i32x4_u"

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
  encodeWat (Plain_FoldedInstr p inputs) = parens $ encodeWat p <+> (hcat . punctuate " " . fmap encodeWat) inputs
  encodeWat (Block_FoldedInstr idM bt body) = parens $ "block" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body))
  encodeWat (Loop_FoldedInstr idM bt body) = parens $ "loop" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (if null body then mempty else encodeWat (Instrs body))
  encodeWat (If_FoldedInstr idM bt inputs thenBody elseBody) = parens $ "if" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (hcat . punctuate " " . fmap encodeWat) inputs <+> parens ("then" <+> (if null thenBody then mempty else encodeWat (Instrs thenBody))) <+> (if null elseBody then mempty else parens $ "else" <+> encodeWat (Instrs elseBody))
  encodeWat (TryTable_FoldedInstr idM bt cs body) = parens $ "try_table" <+> maybe mempty encodeWat idM <+> encodeWat bt <+> (hcat . punctuate " " . fmap encodeWat) cs <+> (if null body then mempty else encodeWat (Instrs body))

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
  encodeWat (Identifier_Idx (Identifier i)) = "$" <> reflow i

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
data Func = Func (Maybe Identifier) (Maybe TypeUse) [Local] Expr
  deriving (Generic, Eq, Show, Ord)

instance EncodeWat Func where
  encodeWat (Func idM tuM ls e) = parens $ "func" <+> maybe mempty encodeWat idM <+> maybe mempty encodeWat tuM <+> (hcat . punctuate " " . fmap encodeWat) ls <+> encodeWat e

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
  encodeWat (ElemList rt es) = encodeWat rt <+> (hcat . punctuate " " . fmap encodeWat) es

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
