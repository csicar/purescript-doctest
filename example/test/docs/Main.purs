module Test.Example.Main where 

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)


-- Imports
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Maybe hiding (Maybe(Just, Nothing))


-- Declarations
f x | x > 0, x < 2 = x + 2
f x = 28


-- Specs
main :: Spec Unit
main = describe "Main" $ do 
    it "value spec in docs from:main" $ show (101 + 2 * (\x ->  
                 x    
              )       
              3) `shouldEqual` "107"
    it "value spec in docs from:main" $ show (23) `shouldEqual` "23"
    it "value spec in docs from:main" $ show (f 1) `shouldEqual` "3"
    it "value spec in docs from:main" $ show (f 23) `shouldEqual` "28"

    pure unit