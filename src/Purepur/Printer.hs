module Purepur.Printer where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Text (Text)
import Debug.Pretty.Simple
import qualified Language.PureScript as P
import Language.PureScript.Interactive.Module (importDecl)
import Language.PureScript.Interactive.Types (Command (..), ImportedModule)
import Language.PureScript.Pretty.Values (prettyPrintValue)
import Purepur.Parser
import Text.PrettyPrint.Boxes (Box, render)
import Prelude

printDeclaration :: Declaration -> Text
printDeclaration (Command cmds) = foldMap printCommand cmds
printDeclaration (ExpectedOutput s) = "**toBe**: " <> s

printCommand :: Command -> Text
printCommand (Expression e) = T.pack $ render $ prettyPrintValue maxBound e
printCommand (Import i@(moduleName, declarationType, maybeQualModName)) =
  "import " <> P.runModuleName moduleName
    <> ( case declarationType of
           P.Implicit -> ""
           P.Explicit decls -> " (" <> printDeclsRefs decls <> ")"
           P.Hiding decls -> " hiding (" <> printDeclsRefs decls <> ")"
       )
    <> maybe "" (" as " <>) (P.runModuleName <$> maybeQualModName)

printDeclsRefs :: [P.DeclarationRef] -> Text
printDeclsRefs decls = T.intercalate ", " (printDeclRef <$> decls)

printDeclRef :: P.DeclarationRef -> Text
printDeclRef (P.TypeRef _ name maybeCtors) =
  P.runProperName name
    <> case maybeCtors of
      Nothing -> "(..)"
      Just [] -> ""
      Just ctors -> "(" <> T.intercalate ", " (P.runProperName <$> ctors) <> ")"

printDeclRef (P.TypeOpRef _ name) = "type " <> P.showOp name
printDeclRef (P.ValueRef _ ident) = P.showIdent ident
printDeclRef (P.ValueOpRef _ op) = P.showOp op
printDeclRef (P.TypeClassRef _ name) = "class " <> P.runProperName name
printDeclRef (P.TypeInstanceRef _ ident) = undefined
