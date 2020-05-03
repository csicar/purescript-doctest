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
--| > 101 + 2 * (\x -> x) 3
--|107
--| > 23
--| 23
--| > f x | x > 0, x < 2 =
--|    x + 2
--|   f x = 28
--| > f 1
--| 3
--| > f 23
--| 28
--|```
main :: Effect Unit
main = do
  log "🍝"
  where tes = 2

f a = 
   a + 2
