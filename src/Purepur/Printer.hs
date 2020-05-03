module Purepur.Printer where

import Control.Arrow ((>>>))
import Data.Function ((&))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Data.Text (Text)
import Debug.Pretty.Simple
import Debug.Trace
import qualified Language.PureScript as P
import Language.PureScript.Docs.Render (renderDeclaration)
import qualified Language.PureScript.Docs.RenderedCode as RenderedCode
import Language.PureScript.Docs.RenderedCode.Types (outputWith)
import Language.PureScript.Interactive.Module (importDecl)
import Language.PureScript.Interactive.Types (Command (..), ImportedModule)
import Language.PureScript.Pretty.Values (prettyPrintValue)
import Purepur.Parser
import Purepur.Types
import Text.PrettyPrint.Boxes (Box, render)
import Prelude

printPurpurDocument :: P.ModuleName -> PurepurDocument -> Text
printPurpurDocument moduleName (PurepurDocument imports decls specs) =
  "module Test.Example." <> P.runModuleName moduleName <> " where \n\n"
  <> T.unlines (printCommand <$> imports) <> "\n\n-- Declarations\n"
  <> T.unlines (printCommand <$> decls) <> "\n\n-- Specs\n"
  <> T.unlines (printSpec <$> specs)
  where
    printSpec :: (Command, Text) -> Text
    printSpec (cmd, expectedOutput) = "it \"example test\" $ show (" <> printCommand cmd <> ") `shouldBe` " <> T.pack (show expectedOutput)

printDeclaration :: CodeFenceCommand -> Text
printDeclaration (Command cmd) = printCommand cmd
printDeclaration (ExpectedOutput s) = "`shouldEqual`" <> T.pack (show s)

printCommand :: Command -> Text
printCommand (Expression e) = printExpression e
printCommand (Import i@(moduleName, declarationType, maybeQualModName)) =
  "import " <> P.runModuleName moduleName
    <> ( case declarationType of
           P.Implicit -> ""
           P.Explicit decls -> " (" <> printDeclsRefs decls <> ")"
           P.Hiding decls -> " hiding (" <> printDeclsRefs decls <> ")"
       )
    <> maybe "" (" as " <>) (P.runModuleName <$> maybeQualModName)
printCommand (Decls decls) = T.intercalate "\n" $ printPursDeclaration <$> decls

printExpression :: P.Expr -> Text
printExpression = prettyPrintValue maxBound >>> render >>> T.pack >>> T.stripEnd

-- `render` always adds "\n" to the end of the string. We use `stripEnd` to remove that

printGuardedExpression :: P.GuardedExpr -> Text
printGuardedExpression (P.GuardedExpr [] expr) =
  -- TODO: print guards
  " = " <> printExpression expr
printGuardedExpression (P.GuardedExpr guards expr) =
  " | "
    <> T.intercalate ", " (printGuard <$> guards)
    <> " = "
    <> printExpression expr

printGuard :: P.Guard -> Text
printGuard (P.ConditionGuard expr) = printExpression expr

printPursDeclaration :: P.Declaration -> Text
printPursDeclaration = \case
  P.DataDeclaration _ dataDeclType typeName textToSourceType dataConstructorDeclarations -> undefined
  P.DataBindingGroupDeclaration declarations -> undefined
  P.TypeSynonymDeclaration _ typeName textToSourceType sourceType -> undefined
  P.TypeDeclaration d -> undefined
  P.ValueDeclaration (P.ValueDeclarationData _ ident nameKind binders guardedExpressions) ->
    P.runIdent ident <> " " <> T.intercalate " " (P.prettyPrintBinder <$> binders) <> foldMap printGuardedExpression guardedExpressions
  -- valDeclData :: ValueDeclarationData [GuardedExpr]
  P.BoundValueDeclaration _ binder expr -> undefined
  P.BindingGroupDeclaration d -> undefined
  P.ExternDeclaration _ ident sourceType -> undefined
  P.ExternDataDeclaration _ typeName sourceType -> undefined
  P.FixityDeclaration _ d -> undefined
  P.ImportDeclaration _ mod imp maybeMod -> undefined
  P.TypeClassDeclaration _ name textToSourceType sourceConstraints funcDeps decls -> undefined
  P.TypeInstanceDeclaration _ idents int ident2 sourceConstraints className sourceTypesypeInstanceBody j -> undefined

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
