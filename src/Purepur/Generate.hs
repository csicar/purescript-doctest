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
import qualified Options.Applicative as Opts
import qualified Purepur.Parser as Parser
import Purepur.Printer (printDeclaration)
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

-- https://hackage.haskell.org/package/purescript-0.13.6/docs/Language-PureScript-Docs-Types.html#t:Declaration
generateTest :: (FilePath, D.Module) -> Except ParseError (P.ModuleName, T.Text)
generateTest (filePath, D.Module name maybeComments declarations _) = do
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
