{-# LANGUAGE StandaloneDeriving #-}

module Purepur.Parser where

import Cheapskate
import Cheapskate.Parse
import Control.Arrow
import Control.Monad.Trans.Except
import Control.Monad.Writer
import Data.Either (fromRight, partitionEithers)
import Data.Foldable
import Data.Maybe (maybeToList)
import Data.Sequence
import Data.Text (Text, isPrefixOf, pack, unpack)
import Debug.Pretty.Simple
import Debug.Trace
import qualified Language.PureScript.AST.Declarations
import qualified Language.PureScript.Interactive.Parser as Psci
import qualified Language.PureScript.Interactive.Types as Psci
import qualified Language.PureScript.Names
import qualified Language.PureScript.TypeClassDictionaries
import Purepur.Types
import Text.ParserCombinators.Parsec
import Prelude

opts = Cheapskate.def {Cheapskate.allowRawHtml = False}

extractCodeFromComment :: Text -> Except ParseError [CodeFenceCommand]
extractCodeFromComment comment =
  foldM (\decl m -> m >>= (\acc -> return $ decl <> acc)) [] $
    except . parseInfoBlock . snd <$> goSeqBlock parsed
  where
    (Doc _ parsed) = Cheapskate.Parse.markdown opts comment
    goSeqBlock :: Blocks -> [(CodeAttr, Text)]
    goSeqBlock = foldMap goBlock
    goBlock :: Block -> [(CodeAttr, Text)]
    goBlock (Blockquote blocks) = goSeqBlock blocks
    goBlock (List _ _ blocks) = foldMap goSeqBlock blocks
    goBlock (CodeBlock codeAttr text) = maybeToList (goCodeBlock codeAttr text)
    goBlock (Para _) = mempty
    goBlock (Header _ _) = mempty
    goBlock (HtmlBlock _) = mempty
    goBlock HRule = mempty
    goCodeBlock :: CodeAttr -> Text -> Maybe (CodeAttr, Text)
    goCodeBlock attr@(CodeAttr "purescript" info) text | "run" `isPrefixOf` info = Just (attr, text)
    goCodeBlock _ _ = Nothing

parseInfoBlock :: Text -> Either ParseError [CodeFenceCommand]
parseInfoBlock = parse parseDeclaration "Error in parsing the code block" . unpack . (<> "\n\n")
  where
    parseDeclaration :: GenParser Char st [CodeFenceCommand]
    parseDeclaration = do
      result <- many parseStatement
      many newline
      eof
      return $ concat result
    --
    textToLineEnd :: GenParser Char st String
    textToLineEnd = many1 $ noneOf "\n"
    --
    parseStatement :: GenParser Char st [CodeFenceCommand]
    parseStatement =
      (char '>' >> (map Command <$> parseCommand) <?> "Command Input")
        <|> ((: []) . ExpectedOutput . pack <$> textToLineEnd <* eolString <?> "Expected Output")
      -- where
        -- resultToString = ((: []) <$>)
    --
    whiteSpace :: GenParser Char st Char
    whiteSpace = char ' ' <|> char '\t'
    --
    parseCommand :: GenParser Char st [Psci.Command]
    parseCommand = do
      many whiteSpace
      commandText <- parseMultilineCommand
      case Psci.parseCommand commandText of
        Left err -> unexpected $ "Psci-Parser: " <> err
        Right parsed -> return parsed
    --
    parseMultilineCommand :: GenParser Char st String
    parseMultilineCommand =
      textToLineEnd
        <> eolString
        <> (mconcat <$> many (string "  " >> textToLineEnd <> eolString))
    --
    eolString = (: []) <$> eol
    eol = char '\n'
