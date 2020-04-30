module Main (main, a, b) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--| asd
--| ```purescript run
--| import Test
--| > f a
--| 10
--| ```
main :: Effect Unit
main = do
  log "🍝"
  where tes = 2

--| ```purescript run
--| a test
--| ```
-- |
a = 1

--| ```pupMrescript run
--| b test
--| ```
-- |
b = 2