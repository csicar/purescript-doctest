module Test.Main where 

import Prelude
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import Test.Example.Main as Main
import Test.Example.ArrayEx as ArrayEx
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- Specs
main = launchAff_ $ runSpec [consoleReporter] do 
  Main.main
  ArrayEx.main
  pure unit