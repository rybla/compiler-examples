module Language.Calculator.Compilation where

import Language.Calculator.Syntax
import Language.Wasm.Wat (Module)
import Utilities.Unsafe (todo)

compile :: Tm an -> Module
compile = todo "compile"
