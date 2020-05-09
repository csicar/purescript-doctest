{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (command, infoModList, main) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad.Trans.Except (Except, runExcept, runExceptT)
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Debug.Pretty.Simple
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import Language.PureScript.Docs.Tags (dumpCtags, dumpEtags)
import qualified Options.Applicative as Opts
import Purepur.Generate
import qualified Purepur.Printer as Printer
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePath.Glob (compile, glob, globDir1, match)
import qualified System.IO as IO
import System.IO (hPutStrLn, stderr)
import System.IO.UTF8 (writeUTF8FileT)
import Text.Parsec (ParseError)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Util.IO.UTF8 (readUTF8FilesT)
import Prelude

data PurepurOptions = PurepurOptions
  { _pscdOutput :: Maybe FilePath,
    _pscdCompileOutputDir :: FilePath,
    -- | Paths of files, that should be used generating for tests
    _purepurInputFiles :: [FilePath],
    -- | Paths of files, that should be used in compilation
    _pscdInputFiles :: [FilePath]
  }
  deriving (Show)

docgen :: PurepurOptions -> IO ()
docgen p@(PurepurOptions moutput compileOutput inputGlob compileInputGlob) = do
  compileInput <- concat <$> mapM glob compileInputGlob
  input <- concat <$> mapM glob inputGlob

  when (null compileInput) $ do
    hPutStrLn stderr "purepur: arguments provides no compile input files."
    exitFailure

  when (null input) $ do
    hPutStrLn stderr "purepur: --src provides no input files."
    exitFailure

  let output = fromMaybe "test/doc-examples" moutput

  fileMs <- parseAndConvert compileInput

  let ext = compile "*.purs"
  msCommentTest <-
    runExceptIO
      $ mapM generateTest
      $ filter ((`elem` input) . fst) fileMs

  markdownInputs <- readUTF8FilesT $ filter ("**/*.md" `match`) input

  testsFromMarkdown <-
    runExceptIO $
      mapM
        generateTestFromMarkdownFile
        markdownInputs

  createDirectoryIfMissing True output
  globDir1 ext output >>= mapM_ removeFile
  writeTestModules output msCommentTest
  writeTestModules output $ (\(doc, name) -> (name, Printer.printPurpurDocument name doc)) <$> testsFromMarkdown

  pure ()
  where
    runExceptIO :: Except ParseError a -> IO a
    runExceptIO = runExcept >>> \case
      Right x -> return x
      Left err -> do
        IO.hPrint stderr err
        exitFailure
    successOrExit :: Either P.MultipleErrors a -> IO a
    successOrExit act =
      case act of
        Right x ->
          return x
        Left err -> do
          hPutStrLn stderr $ P.prettyPrintMultipleErrors P.defaultPPEOptions err
          exitFailure
    parseAndConvert input =
      runExceptT (fmap fst (D.collectDocs compileOutput input []))
        >>= successOrExit

writeTestModules :: FilePath -> [(P.ModuleName, T.Text)] -> IO ()
writeTestModules outputDir = mapM_ (writeTestModule outputDir)

writeTestModule :: FilePath -> (P.ModuleName, T.Text) -> IO ()
writeTestModule outputDir (mn, text) = do
  let filepath = outputDir ++ "/" ++ T.unpack (P.runModuleName mn) ++ ".purs"
  writeUTF8FileT filepath text

main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cmd <- Opts.execParser opts
  cmd
  where
    opts =
      Opts.info
        command
        ( Opts.progDesc "Print a greeting for TARGET"
            <> Opts.header "hello - a test for optparse-applicative"
        )

pscDocsOptions :: Opts.Parser PurepurOptions
pscDocsOptions = PurepurOptions <$> output <*> compileOutputDir <*> some purepurInputFile <*> many inputFile
  where
    output :: Opts.Parser (Maybe FilePath)
    output =
      optional $ Opts.strOption $
        Opts.long "output"
          <> Opts.short 'o'
          <> Opts.metavar "DEST"
          <> Opts.help "File/directory path for docs to be written to"
    compileOutputDir :: Opts.Parser FilePath
    compileOutputDir =
      Opts.strOption $
        Opts.value "output"
          <> Opts.showDefault
          <> Opts.long "compile-output"
          <> Opts.metavar "DIR"
          <> Opts.help "Compiler output directory"
    purepurInputFile :: Opts.Parser FilePath
    purepurInputFile =
      Opts.strOption $
        Opts.long "src"
          <> Opts.metavar "FILE"
          <> Opts.help "The files, for which tests should be generated (should only include source files)"
    inputFile :: Opts.Parser FilePath
    inputFile =
      Opts.strArgument $
        Opts.metavar "FILE"
          <> Opts.help "The input .purs file(s) used for compilation (should include dependencies)"

command :: Opts.Parser (IO ())
command = docgen <$> (Opts.helper <*> pscDocsOptions)

infoModList :: Opts.InfoMod a
infoModList = Opts.fullDesc <> footerInfo
  where
    footerInfo = Opts.footerDoc $ Just examples

examples :: PP.Doc
examples =
  PP.vcat $
    map
      PP.text
      [ "Examples:",
        "  write documentation for all modules to ./generated-docs:",
        "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\"",
        "",
        "  write documentation in Markdown format for all modules to ./generated-docs:",
        "    purs docs --format markdown \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\"",
        "",
        "  write CTags to ./tags:",
        "    purs docs --format ctags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\"",
        "",
        "  write ETags to ./TAGS:",
        "    purs docs --format etags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
      ]
