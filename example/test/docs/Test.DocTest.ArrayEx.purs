module Test.DocTest.ArrayEx where 

import Prelude
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)


-- Imports
import Data.Array
import Data.Maybe (Maybe(..))


-- Declarations
asMaybeInt :: Maybe Int -> Maybe Int
asMaybeInt a = a


-- Specs
main :: Spec Unit
main = describe "Test.DocTest.ArrayEx" $ do 
    it "value spec in docs from: main" $ show (fromFoldable (Just 1)) `shouldEqual` "[1]"
    it "value spec in docs from: main" $ show (fromFoldable (Nothing :: Maybe Int)) `shouldEqual` "[]"
    it "type spec in docs from: main" $ do
        let testType = (
            [ [ 1
              ]  
            ]) :: (Array (Array Int))
        pure unit
    
    it "value spec in docs from: main" $ show (singleton 2) `shouldEqual` "[2]"

    pure unit