module Purepur.Generate where


import Prelude

import           Control.Applicative
import           Control.Monad.Writer
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import           Language.PureScript.Docs.Tags (dumpCtags, dumpEtags)
import qualified Options.Applicative as Opts
import qualified System.IO as IO
import Purepur.Printer (printDeclaration)

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeFile)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.FilePath.Glob (compile, glob, globDir1)
import           System.IO (hPutStrLn, stderr)
import           System.IO.UTF8 (writeUTF8FileT)
import Control.Monad.Trans.Except
import Debug.Pretty.Simple
import qualified Purepur.Parser as Parser
import Text.Parsec (ParseError)

-- https://hackage.haskell.org/package/purescript-0.13.6/docs/Language-PureScript-Docs-Types.html#t:Declaration
generateTest :: (FilePath, D.Module) -> Except ParseError (P.ModuleName, T.Text)
generateTest (filePath, D.Module name maybeComments declarations _)= do 

  moduleComment <- toTest maybeComments
  declarationComments <- concat <$> mapM generateTestForDeclaration declarations

  -- let document = T.unlines $ toTest maybeComments <> mconcat (map generateTestForDeclaration declarations)


  return (name, documentHeader <> T.unlines (moduleComment ++ declarationComments))
  where

    documentHeader :: T.Text
    documentHeader = "module Test.Example." <> P.runModuleName name <> " where \n\n"

    toTest :: Maybe T.Text -> Except ParseError [T.Text]
    toTest Nothing = return []
    toTest (Just comment) = do
      declarations <- Parser.extractCodeFromComment comment
      return $ printDeclaration <$> declarations

    generateTestForDeclaration (D.Declaration title maybeComment _ childDecl _) = do
      ownComments <- toTest maybeComment
      childComments <- concat <$> mapM generateTestForChildDeclaration childDecl
      return $ ownComments ++ childComments

    generateTestForChildDeclaration (D.ChildDeclaration title maybeComment _ _) = toTest maybeComment