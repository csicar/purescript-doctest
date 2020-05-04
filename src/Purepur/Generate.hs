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
generateTest (filePath, D.Module name maybeComments declarations _) = do
  moduleComment <- toTest (P.runModuleName name) maybeComments
  declarationComments <- mconcat <$> mapM generateTestForDeclaration declarations

  return (name, printPurpurDocument name $ moduleComment <> declarationComments)
  where
    toTest :: T.Text -> Maybe T.Text -> Except ParseError PurepurDocument
    toTest _ Nothing = return mempty
    toTest title (Just comment) = do
      declarations <- Parser.extractCodeFromComment comment

      return $ commandsToDocument title declarations
    --
    generateTestForDeclaration :: D.Declaration -> Except ParseError PurepurDocument
    generateTestForDeclaration (D.Declaration title maybeComment _ childDecl _) = do
      ownComments <- toTest title maybeComment
      childComments <- mconcat <$> mapM generateTestForChildDeclaration childDecl
      return $ ownComments <> childComments
    generateTestForChildDeclaration (D.ChildDeclaration title maybeComment _ _) = toTest title maybeComment

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