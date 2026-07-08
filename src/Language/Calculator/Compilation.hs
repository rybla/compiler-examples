module Language.Calculator.Compilation where

import Language.Calculator.Syntax
import Language.Wasm.Wat (Wat)
import Utilities.Unsafe (todo)

compile :: Tm an -> Wat
compile = todo "compile"
