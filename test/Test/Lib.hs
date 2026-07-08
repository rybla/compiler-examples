{-# LANGUAGE TemplateHaskell #-}

module Test.Lib (spec) where

import Control.Lens ((^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax qualified as TH
import Lib (greet)
import System.FilePath (joinPath)
import System.FilePath.Glob (glob)
import System.FilePath.Lens (basename, directory, (</>~))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

spec :: TestTree
spec =
  testGroup "Lib" . concat $
    [ $( glob (joinPath ["asset", "example", "*.txt"])
           & runIO
           >>= TH.lift
       )
        <&> ( \inputFilepath ->
                goldenVsString
                  (inputFilepath ^. basename)
                  (inputFilepath & directory </>~ "output")
                  $ do
                    input <- TLIO.readFile inputFilepath
                    let output = greet input
                    return . TL.encodeUtf8 $ output
            )
    ]
