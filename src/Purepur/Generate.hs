{-# LANGUAGE OverloadedStrings #-}

module Purepur.Generate where

import Control.Applicative
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Debug.Pretty.Simple
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Language.PureScript.Docs.Tags (dumpCtags, dumpEtags)
import qualified Language.PureScript.Interactive as Psci
import qualified Options.Applicative as Opts
import qualified Purepur.Parser as Parser
import Purepur.Printer (printPurpurDocument)
import Purepur.Types
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, glob, globDir1)
import qualified System.IO as IO
import System.IO (hPutStrLn, stderr)
import System.IO.UTF8 (writeUTF8FileT)
import Text.Parsec (ParseError)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Prelude

generateTest :: (FilePath, D.Module) -> Except ParseError (P.ModuleName, T.Text)
generateTest (filePath, D.Module (P.ModuleName sourceModName) maybeComments declarations _) = do
  let name = P.moduleNameFromString $ "Test.DocTest." <> sourceModName
  moduleComment <- generateTestFromMarkdown (P.runModuleName name) maybeComments
  declarationComments <- mconcat <$> mapM generateTestForDeclaration declarations

  return (name, printPurpurDocument name $ moduleComment <> declarationComments)
  where
    --
    generateTestForDeclaration :: D.Declaration -> Except ParseError PurepurDocument
    generateTestForDeclaration (D.Declaration title maybeComment _ childDecl _) = do
      ownComments <- generateTestFromMarkdown title maybeComment
      childComments <- mconcat <$> mapM generateTestForChildDeclaration childDecl
      return $ ownComments <> childComments
    generateTestForChildDeclaration (D.ChildDeclaration title maybeComment _ _) = generateTestFromMarkdown title maybeComment

generateTestFromMarkdown :: T.Text -> Maybe T.Text -> Except ParseError PurepurDocument
generateTestFromMarkdown _ Nothing = return mempty
generateTestFromMarkdown title (Just comment) = do
  declarations <- Parser.extractCodeFromComment comment

  return $ commandsToDocument title declarations
  where
    commandsToDocument :: T.Text -> [CodeFenceCommand] -> PurepurDocument
    commandsToDocument title (Command (Psci.Import i) : rest) =
      documentFromImport i <> commandsToDocument title rest
    commandsToDocument title (Command (Psci.Expression expr) : ExpectedOutput str : rest) =
      documentFromSpec (ValueSpec title expr str) <> commandsToDocument title rest
    commandsToDocument title (Command (Psci.TypeOf expr) : ExpectedOutput str : rest) =
      documentFromSpec (TypeSpec title expr str) <> commandsToDocument title rest
    commandsToDocument title (Command (Psci.Decls decls) : rest) =
      documentFromDecl decls <> commandsToDocument title rest
    commandsToDocument title (Command (Psci.Expression expr) : rest) =
      --TODO: warn user about ignored expr
      error $ "unused expression: " <> show expr
    commandsToDocument title [] = mempty
    commandsToDocument title (ExpectedOutput str: rest) = 
      error $ "Could not translate doctest comment: Found expected output " <> show str <> ", but no matching command."
        <> " Maybe you missed a `>`? "

generateTestFromMarkdownFile :: (FilePath, T.Text) -> Except ParseError (PurepurDocument, P.ModuleName)
generateTestFromMarkdownFile (path, textContent) = do
  purepurDocument <- generateTestFromMarkdown (T.pack path) (Just textContent)
  return (purepurDocument, moduleNameFromPath path)
  where
    moduleNameFromPath :: FilePath -> P.ModuleName
    moduleNameFromPath filePath =
      P.moduleNameFromString $
        "Test.MarkdownExamples."
          <> T.intercalate
            "."
            ( filter (/= "")
                $ T.splitOn "/"
                $ T.replace "." ""
                $ T.pack (pTraceShowId filePath)
            )

-- generates a File, that imports all others tests and runs them.
generateSummaryFile :: [P.ModuleName] -> (PurepurDocument, P.ModuleName)
generateSummaryFile testModules = (doc, P.moduleNameFromString "DocTest")
  where
    doc :: PurepurDocument
    doc = mconcat $ documentFromModuleName <$> testModules
    documentFromModuleName :: P.ModuleName -> PurepurDocument
    documentFromModuleName mod =
      documentFromImport (mod, P.Implicit, Just mod)
        <> documentFromSpec (ReferenceSpec $ P.mkQualified (P.Ident "main") mod)
