module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

--| asd
--| ```purescript run
--| import Tests
--|as
--| > f a
--| 10
--|
--|
--| ```
main :: Effect Unit
main = do
  log "🍝"
  where tes = 2

--| asd
--| ```purescript run
--| > import Test
--|as
--| > f a
--| 10
--|
--|
--| ```
a = 2