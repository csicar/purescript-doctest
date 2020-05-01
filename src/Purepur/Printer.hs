module Purepur.Printer where


import Purepur.Parser
import Data.Text
import Prelude


printDeclaration :: Declaration -> Text
printDeclaration (Import s) = "import " <> s
printDeclaration (Command s) = s
printDeclaration (ExpectedOutput s) = s