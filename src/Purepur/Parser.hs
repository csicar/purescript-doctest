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
import Text.ParserCombinators.Parsec.Token (whiteSpace)
import Control.Monad.Trans.Except

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
  = Import Text
  | Command Text
  | ExpectedOutput Text deriving (Show)


parseInfoBlock :: Text -> Either ParseError [Declaration]
parseInfoBlock = parse parseDeclaration "parse error" . unpack . (<> "\n")
  where 
    parseDeclaration :: GenParser Char st [Declaration]
    parseDeclaration = do
        result <- many parseStatement
        many eol
        eof
        return result

    textToLineEnd :: GenParser Char st String
    textToLineEnd = many1 $ noneOf "\n"

    parseStatement :: GenParser Char st Declaration
    parseStatement = 
      ((Import . pack <$> (string "import" >> many  whiteSpace  >> textToLineEnd))
      <|> (Command . pack <$> (string ">" >> many  whiteSpace >> textToLineEnd))
      <|> (ExpectedOutput . pack <$> (many whiteSpace >> textToLineEnd)))
      <* eol

    eol = char '\n'

    whiteSpace :: GenParser Char st Char
    whiteSpace = char ' ' <|> char '\t'
