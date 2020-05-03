module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--| asd
--| ```purescript run
--| > import Data.Array as A
--| > import Data.Maybe (Maybe(..))
--| > import Data.Maybe hiding (Maybe(Just, Nothing))
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
