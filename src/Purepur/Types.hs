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
  { imports :: [Psci.ImportedModule],
    declarations :: [P.Declaration],
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
  | ReferenceSpec (P.Qualified P.Ident)
  deriving (Show)

documentFromImport :: Psci.ImportedModule -> PurepurDocument
documentFromImport i = PurepurDocument [i] [] []

documentFromDecl :: [P.Declaration] -> PurepurDocument
documentFromDecl i = PurepurDocument [] i []

documentFromSpec :: PurepurSpec -> PurepurDocument
documentFromSpec spec = PurepurDocument [] [] [spec]
