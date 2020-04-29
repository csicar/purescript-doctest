module Main (main, a, b) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--| asd
--| ```purescript
--| import Test
--| > f a
--| 10
--| ```
main :: Effect Unit
main = do
  log "🍝"
  where tes = 2

--| ```purescript
--| a test
--| ```
-- |
a = 1

--| ```purescript
--| b test
--| ```
-- |
b = 2