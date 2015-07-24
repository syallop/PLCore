{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PL.Name where

import Data.Text
import GHC.Exts (IsString(..))

-- | Name a thing
type Name = Text

-- | Name a Type
newtype TypeName = TypeName {unTypeName :: Name} deriving (Eq,Ord,IsString)

-- | Name a Term
newtype TermName = TermName {unTermName :: Name} deriving (Eq,Ord,IsString)

instance Show TypeName where show (TypeName n) = unpack n
instance Show TermName where show (TermName n) = unpack n

