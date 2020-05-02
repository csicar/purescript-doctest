module Purepur.Printer where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Language.PureScript as P
import Language.PureScript.Interactive.Module (importDecl)
import Language.PureScript.Interactive.Types (Command (..), ImportedModule)
import Language.PureScript.Pretty.Values (prettyPrintValue)
import Purepur.Parser
import Text.PrettyPrint.Boxes (render)
import Prelude

printDeclaration :: Declaration -> Text
printDeclaration (Command cmds) = T.pack $ foldMap printCommand cmds
--
printDeclaration (ExpectedOutput s) = s

printCommand :: Command -> String
printCommand (Expression e) = render $ prettyPrintValue maxBound e
printCommand (Import i@(moduleName, declarationType, maybeQualModName)) = show $ importDecl i
