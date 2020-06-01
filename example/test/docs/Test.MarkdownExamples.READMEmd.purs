module Test.MarkdownExamples.READMEmd where 

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)


-- Imports


-- Declarations
f x = x


-- Specs
main :: Spec Unit
main = describe "Test.MarkdownExamples.READMEmd" $ do 
    it "value spec in docs from: ./README.md" $ show (f 2) `shouldEqual` "2"

    pure unit