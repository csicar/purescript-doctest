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
import Purepur.Parser
import Purepur.PurescriptPrettyPrinter (prettyPrintValue)
import Purepur.Types
import Text.PrettyPrint.Boxes (Box, render)
import Prelude

printPurpurDocument :: P.ModuleName -> PurepurDocument -> Text
printPurpurDocument moduleName (PurepurDocument imports decls specs) =
  "module " <> P.runModuleName moduleName <> " where \n\n" <>
    T.concat 
    [ T.unlines $ ("import " <>) <$> defImports
    , "\n\n-- Imports\n"
    , T.unlines $ printImport <$> imports
    , "\n\n-- Declarations\n"
    , T.unlines $  printPursDeclaration <$> decls
    , "\n\n-- Specs\n"
    , "main :: Spec Unit\n"
    , "main = describe "
    , T.pack $ show $ P.runModuleName moduleName
    , " $ do \n"
    , indent $ T.unlines (printSpec <$> specs)
    , "\n    pure unit"
    ]
  where
    printSpec :: PurepurSpec -> Text
    printSpec (ValueSpec title cmd expectedOutput) = "it \"value spec in docs from: " <> title <> "\" $ show (" <> printExpression cmd <> ") `shouldEqual` " <> T.pack (show expectedOutput)
    printSpec (ReferenceSpec ref) = printExpression (P.Var P.nullSourceSpan ref)
    printSpec (TypeSpec title cmd ty) =
      "it \"type spec in docs from: "<> title<> "\" $ do\n"
        <> indent
          ( "let testType = (\n" <> indent (printExpression cmd <> ") :: (" <> ty <> ")\n")
              <> "pure unit"
          )
    indent :: Text -> Text
    indent = T.lines >>> fmap ("    " <>) >>> T.unlines
    defImports :: [Text]
    defImports =
      [ "Prelude",
        "Test.Spec (describe, it, Spec)",
        "Test.Spec.Assertions (shouldEqual)"
      ]

printImport i@(moduleName, declarationType, maybeQualModName) =
  "import " <> P.runModuleName moduleName
    <> ( case declarationType of
           P.Implicit -> ""
           P.Explicit decls -> " (" <> printDeclsRefs decls <> ")"
           P.Hiding decls -> " hiding (" <> printDeclsRefs decls <> ")"
       )
    <> maybe "" (" as " <>) (P.runModuleName <$> maybeQualModName)

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
  P.TypeDeclaration (P.TypeDeclarationData _ ident ty) -> P.runIdent ident <> " :: " <> T.stripEnd (T.pack $ P.prettyPrintType maxBound ty)
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
