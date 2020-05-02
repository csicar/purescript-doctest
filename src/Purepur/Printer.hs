module Purepur.Printer where


import Purepur.Parser
import Data.Text
import Prelude


printDeclaration :: Declaration -> Text
printDeclaration (Command s) = pack $ show s
printDeclaration (ExpectedOutput s) = s