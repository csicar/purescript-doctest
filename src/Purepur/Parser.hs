{-# LANGUAGE StandaloneDeriving #-}
module Purepur.Parser where

import Prelude
import Cheapskate.Parse
import Cheapskate
import Data.Text (Text, unpack, pack, isPrefixOf)
import Control.Monad.Writer
import Data.Sequence
import Data.Foldable
import Data.Maybe (maybeToList)
import Text.ParserCombinators.Parsec
import Data.Either (fromRight, partitionEithers)
import Control.Arrow
import Debug.Pretty.Simple
import Control.Monad.Trans.Except
import qualified Language.PureScript.Interactive.Types as Psci
import qualified Language.PureScript.Interactive.Parser as Psci
import qualified Language.PureScript.AST.Declarations
import qualified Language.PureScript.TypeClassDictionaries
import qualified Language.PureScript.Names
import Debug.Trace

opts = Cheapskate.def { Cheapskate.allowRawHtml = False }

extractCodeFromComment :: Text -> Except ParseError [Declaration]
extractCodeFromComment comment =  foldM (\decl m -> m >>= (\acc -> return $ decl <> acc)) [] 
  $ except . pTraceShowId . parseInfoBlock . snd <$> goSeqBlock parsed
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

data Declaration
  = Command [Psci.Command]
  | ExpectedOutput Text deriving (Show)

instance Eq Declaration where
  a == b = show a == show b -- TODO : Eq instance for Psci.Command missing. Fix this in Purescript lang

parseInfoBlock :: Text -> Either ParseError [Declaration]
parseInfoBlock = parse parseDeclaration "Error in parsing the code block" . unpack . (<> "\n") . pTraceShowId
  where
  parseDeclaration :: GenParser Char st [Declaration]
  parseDeclaration = do
      result <- many parseStatement
      many newline
      eof
      return result

  textToLineEnd :: GenParser Char st String
  textToLineEnd = many1 $ noneOf "\n"

  parseStatement :: GenParser Char st Declaration
  parseStatement =
    (Command <$> (char '>' >> parseCommand))
    <|> (ExpectedOutput . pack <$> resultToString (noneOf " \n") <> textToLineEnd)

    where
      resultToString = ((: []) <$>)

  whiteSpace :: GenParser Char st Char
  whiteSpace = char ' ' <|> char '\t'

  parseCommand :: GenParser Char st [Psci.Command]
  parseCommand = do
    many whiteSpace
    commandText <- parseMultilineCommand
    case Psci.parseCommand (pTraceShow ("parseCommand", commandText) commandText) of
      Left err -> unexpected $ "Psci-Parser: " <> err
      Right parsed -> return parsed

  parseMultilineCommand :: GenParser Char st String
  parseMultilineCommand = 
       textToLineEnd 
    <> eolString 
    <> (mconcat <$> many (string "  " >> textToLineEnd <> eolString))

  eolString = (: []) <$> eol

  eol = char '\n'