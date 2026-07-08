import Test.Lib qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup "haskell-agentic-stack-template" $
    [ Test.Lib.spec
    ]
