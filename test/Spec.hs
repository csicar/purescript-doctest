{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Purepur.Parser
import qualified Purepur.Printer as Printer
import Data.Text as T
import qualified Language.PureScript.Names as Purs
import qualified Language.PureScript.AST as AST
import Purepur.Types
import qualified Language.PureScript.Interactive.Types as Psci 


main :: IO ()
main = hspec $ do
  describe "Parse Comment" $ do
    let importT = Command $ Psci.Import (Purs.moduleNameFromString "T", AST.Implicit, Nothing)

    it "simple output" $ 
      parseInfoBlock "asd" `shouldBe` Right [ExpectedOutput "asd"]

    it "simple command" $ 
      parseInfoBlock "> import T" `shouldBe` Right [importT]

    it "multiline command" $
      parseInfoBlock "> import\n   T" `shouldBe`  Right [importT]

    it "multi-statement" $
      parseInfoBlock "> import\n   T\n123" `shouldBe` Right [importT, ExpectedOutput "123"]

  describe "Pretty Print" $ do
    it "print do block" $ do
      let Right [doBlock, expectedOutput] = parseInfoBlock "> do\n    a\n    b\n    c\nunit"
      case doBlock of
        Command (Psci.Expression e) -> 
          Printer.printExpression e `shouldBe` "do  \n   a\n   b\n   c"