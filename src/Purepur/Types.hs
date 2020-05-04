module Purepur.Types where

import Data.Text (Text)
import qualified Language.PureScript as P
import qualified Language.PureScript.Interactive as Psci
import Prelude

data CodeFenceCommand
  = Command Psci.Command
  | ExpectedOutput Text
  deriving (Show)

instance Eq CodeFenceCommand where
  a == b = show a == show b -- TODO : Eq instance for Psci.Command missing. Fix this in Purescript lang

data PurepurDocument = PurepurDocument
  { imports :: [Psci.Command],
    declarations :: [Psci.Command],
    specs :: [PurepurSpec]
  }
  deriving (Show)

instance Semigroup PurepurDocument where
  (PurepurDocument a b c) <> (PurepurDocument a' b' c') = PurepurDocument (a <> a') (b <> b') (c <> c')

instance Monoid PurepurDocument where
  mempty = PurepurDocument [] [] []

data PurepurSpec
  = ValueSpec Text P.Expr Text
  | TypeSpec Text P.Expr Text
  deriving (Show)

documentFromImport :: Psci.ImportedModule -> PurepurDocument
documentFromImport i = PurepurDocument [Psci.Import i] [] []

documentFromDecl :: [P.Declaration] -> PurepurDocument
documentFromDecl i = PurepurDocument [] [Psci.Decls i] []

documentFromSpec :: PurepurSpec -> PurepurDocument
documentFromSpec spec = PurepurDocument [] [] [spec]
