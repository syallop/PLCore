{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  #-}
{-|
Module      : PL.Name
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Assign new name types to things we really wouldnt want to accidentally confuse.
Type and variable names for example.
-}
module PL.Name where

import Data.Monoid
import Data.Text
import GHC.Exts (IsString(..))

import PL.Printer

-- | Name a thing
type Name = Text

-- | Name a Type
newtype TypeName = TypeName {unTypeName :: Name} deriving (Eq,Ord,IsString)

-- | Name a Term
newtype TermName = TermName {unTermName :: Name} deriving (Eq,Ord,IsString)

instance Show TypeName where show (TypeName n) = unpack n
instance Show TermName where show (TermName n) = unpack n

instance Document TypeName where document (TypeName n) = char '#' <> text n
instance Document TermName where document (TermName n) = char '#' <> text n

