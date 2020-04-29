module Purepur.Parser where

import Prelude
import Cheapskate.Parse
import Cheapskate
import Data.Text
import Control.Monad.Writer
import Data.Sequence
import Data.Foldable

opts = Cheapskate.def { Cheapskate.allowRawHtml = False }

extractCodeFromComment :: Text -> [Text]
extractCodeFromComment comment = snd <$> goSeqBlock parsed
  where
    (Doc _ parsed) = Cheapskate.Parse.markdown opts comment

    goSeqBlock :: Blocks -> [(CodeAttr, Text)]
    goSeqBlock = foldMap goBlock

    goBlock :: Block -> [(CodeAttr, Text)]
    goBlock (Blockquote blocks) = goSeqBlock blocks
    goBlock (List _ _ blocks) = foldMap goSeqBlock blocks
    goBlock (CodeBlock codeAttr text) = [(codeAttr, text)]
    goBlock (Para _) = mempty
    goBlock (Header _ _) = mempty
    goBlock (HtmlBlock _) = mempty
    goBlock HRule = mempty