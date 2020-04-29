{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (command, infoModList, main) where

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
import Purepur.Generate

-- | Available output formats
data Format
  = Markdown
  | Html
  | Ctags -- Output ctags symbol index suitable for use with vi
  | Etags -- Output etags symbol index suitable for use with emacs
  deriving (Show, Eq, Ord)



data PSCDocsOptions = PSCDocsOptions
  { _pscdFormat :: Format
  , _pscdOutput :: Maybe FilePath
  , _pscdCompileOutputDir :: FilePath
  , _pscdInputFiles  :: [FilePath]
  }
  deriving (Show)

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt moutput compileOutput inputGlob) = do
  input <- concat <$> mapM glob inputGlob
  when (null input) $ do
    hPutStrLn stderr "purs docs: no input files."
    exitFailure

  let output = fromMaybe (defaultOutputForFormat fmt) moutput

  fileMs <- parseAndConvert input

  let ext = compile "*.purs"
  let msCommentTest = map generateTest fileMs
  createDirectoryIfMissing True output
  globDir1 ext output >>= mapM_ removeFile
  writeTestModules output msCommentTest

  pure ()

  where
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

  writeTagsToFile :: String -> [String] -> IO ()
  writeTagsToFile outputFilename tags = do
    currentDir <- getCurrentDirectory
    let outputFile = currentDir </> outputFilename
    let text = T.pack . unlines $ tags
    writeUTF8FileT outputFile text


writeTestModules :: FilePath -> [(P.ModuleName, T.Text)] -> IO ()
writeTestModules outputDir modules = do
  mapM_ (writeTestModule outputDir ) modules

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
  opts = Opts.info (command)
    ( Opts.progDesc "Print a greeting for TARGET"
    <> Opts.header "hello - a test for optparse-applicative" )

instance Read Format where
  readsPrec _ "etags" = [(Etags, "")]
  readsPrec _ "ctags" = [(Ctags, "")]
  readsPrec _ "markdown" = [(Markdown, "")]
  readsPrec _ "html" = [(Html, "")]
  readsPrec _ _ = []

defaultOutputForFormat :: Format -> FilePath
defaultOutputForFormat fmt = "generated-docs/html"

pscDocsOptions :: Opts.Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> format <*> output <*> compileOutputDir <*> many inputFile
  where
  format :: Opts.Parser Format
  format = Opts.option Opts.auto $
       Opts.value Html
    <> Opts.long "format"
    <> Opts.metavar "FORMAT"
    <> Opts.help "Set output FORMAT (markdown | html | etags | ctags)"

  output :: Opts.Parser (Maybe FilePath)
  output = optional $ Opts.strOption $
       Opts.long "output"
    <> Opts.short 'o'
    <> Opts.metavar "DEST"
    <> Opts.help "File/directory path for docs to be written to"

  compileOutputDir :: Opts.Parser FilePath
  compileOutputDir = Opts.strOption $
       Opts.value "output"
    <> Opts.showDefault
    <> Opts.long "compile-output"
    <> Opts.metavar "DIR"
    <> Opts.help "Compiler output directory"

  inputFile :: Opts.Parser FilePath
  inputFile = Opts.strArgument $
       Opts.metavar "FILE"
    <> Opts.help "The input .purs file(s)"

command :: Opts.Parser (IO ())
command = docgen <$> (Opts.helper <*> pscDocsOptions)

infoModList :: Opts.InfoMod a
infoModList = Opts.fullDesc <> footerInfo where
  footerInfo = Opts.footerDoc $ Just examples

examples :: PP.Doc
examples =
  PP.vcat $ map PP.text
    [ "Examples:"
    , "  write documentation for all modules to ./generated-docs:"
    , "    purs docs \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write documentation in Markdown format for all modules to ./generated-docs:"
    , "    purs docs --format markdown \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write CTags to ./tags:"
    , "    purs docs --format ctags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    , ""
    , "  write ETags to ./TAGS:"
    , "    purs docs --format etags \"src/**/*.purs\" \".psc-package/*/*/*/src/**/*.purs\""
    ]
