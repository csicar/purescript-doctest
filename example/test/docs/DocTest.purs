module DocTest where 

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)


-- Imports
import Test.DocTest.ArrayEx as Test.DocTest.ArrayEx
import Test.DocTest.Main as Test.DocTest.Main
import Test.DocTest.Sub.Sub1 as Test.DocTest.Sub.Sub1
import Test.MarkdownExamples.READMEmd as Test.MarkdownExamples.READMEmd


-- Declarations


-- Specs
main :: Spec Unit
main = describe "DocTest" $ do 
    Test.DocTest.ArrayEx.main
    Test.DocTest.Main.main
    Test.DocTest.Sub.Sub1.main
    Test.MarkdownExamples.READMEmd.main

    pure unit