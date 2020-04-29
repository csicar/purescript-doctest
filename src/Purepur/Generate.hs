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

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Directory (getCurrentDirectory, createDirectoryIfMissing, removeFile)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.FilePath.Glob (compile, glob, globDir1)
import           System.IO (hPutStrLn, stderr)
import           System.IO.UTF8 (writeUTF8FileT)
import Debug.Pretty.Simple
import qualified Purepur.Parser as Parser

-- https://hackage.haskell.org/package/purescript-0.13.6/docs/Language-PureScript-Docs-Types.html#t:Declaration
generateTest :: (FilePath, D.Module) -> (P.ModuleName, T.Text)
generateTest (filePath, D.Module name maybeComments declarations _)= (name, T.unlines $ toTest maybeComments <> mconcat (map generateTestForDeclaration declarations))
  where
    toTest Nothing = []
    toTest (Just comment) = Parser.extractCodeFromComment comment

    generateTestForDeclaration (D.Declaration title maybeComment _ childDecl _) = toTest maybeComment <> mconcat (map generateTestForChildDeclaration childDecl)

    generateTestForChildDeclaration (D.ChildDeclaration title maybeComment _ _) = toTest maybeComment