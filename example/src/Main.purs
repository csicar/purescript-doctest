module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--| asd
--| ```purescript run
--| > import Tests as T
--| > import Test2 (asd, class Test, (*), type (*), Maybe(..))
--| > import Test3 hiding (ads, Maybe, Maybe(Just, Nothing))
--| > import Asd
--| > 101 + 2 * (\x -> [x])
--|102
--| > 23
--|```
main :: Effect Unit
main = do
  log "🍝"
  where tes = 2

f a = 
   a + 2
