{- HLINT ignore "Redundant $" -}

module Language.Test.Wasm.Wat (spec) where

import Control.Monad ((<=<))
import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding qualified as LazyTextEncoding
import Language.Wasm.Wat
import Language.Wasm.Wat.Utilities (formatWat, toWasm)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Text.Lazy (readProcessWithExitCode)
import Test.Tasty (DependencyType (AllSucceed), TestName, TestTree, after, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Wat" $
    [ testWat "x1-return_default" $
        Module
          Nothing
          [ Type_Decl $
              Type . RecType $
                [ TypeDef
                    (Just . Identifier $ "return_default_type")
                    ( SubType
                        Nothing
                        []
                        ( Func_CompType
                            []
                            [Result . NumType_ValType $ I32_NumType]
                        )
                    )
                ],
            Func_Decl $
              Func
                (Just . Identifier $ "return_default")
                (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "return_default_type")
                [Local (Just . Identifier $ "x") (NumType_ValType I32_NumType)]
                ( Expr
                    [ Plain_Instr . LocalGet_PlainInstr . LocalIdx . Identifier_Idx . Identifier $ "x"
                    ]
                ),
            Export_Decl $
              Export
                (Name "main")
                (Func_ExternIdx $ FuncIdx $ Identifier_Idx $ Identifier $ "main"),
            Type_Decl $
              Type . RecType $
                [ TypeDef
                    (Just . Identifier $ "main_type")
                    ( SubType
                        Nothing
                        []
                        ( Func_CompType
                            []
                            [Result . NumType_ValType $ I32_NumType]
                        )
                    )
                ],
            Func_Decl $
              Func
                (Just . Identifier $ "main")
                (Just . TypeUse . TypeIdx . Identifier_Idx . Identifier $ "main_type")
                []
                ( Expr
                    [ Plain_Instr . Call_PlainInstr . FuncIdx . Identifier_Idx . Identifier $ "return_default"
                    ]
                )
          ]
    ]

testWat :: TestName -> Module -> TestTree
testWat name m =
  testGroup name $
    [ goldenVsString "wat" ("asset/golden/Wat/" <> name <> ".golden.wat") $ encodeWatAsUtf8 m,
      after AllSucceed (name <> ".wat") $ goldenVsString "wasm" ("asset/golden/Wat/" <> name <> ".golden.wasm") $ encodeWatAsWasm m,
      after AllSucceed (name <> ".wasm") $ goldenVsString "interp" ("asset/golden/Wat/" <> name <> ".out.golden.txt") $ interpretWasm ("asset/golden/Wat/" <> name <> ".golden.wasm")
    ]

encodeWatAsUtf8 :: Module -> IO LazyByteString
encodeWatAsUtf8 =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . formatWat

encodeWatAsWasm :: Module -> IO LazyByteString
encodeWatAsWasm =
  either
    (fail . LazyText.unpack . ("Invalid WAT: " <>))
    (pure . LazyTextEncoding.encodeUtf8)
    <=< runExceptT
      . toWasm

interpretWasm :: FilePath -> IO LazyByteString
interpretWasm fp = do
  (errorCode, out, err) <- readProcessWithExitCode "wasmtime" ["run", "-W", "gc=y", "--invoke", "main", fp] ""
  case errorCode of
    ExitFailure _ -> pure . encodeUtf8 $ "Error\n\n" <> err
    ExitSuccess -> pure . encodeUtf8 $ out
