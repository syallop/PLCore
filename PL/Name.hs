{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PL.Name where

import GHC.Exts (IsString(..))

-- | Name a thing
type Name = String

-- | Name a Type
newtype TypeName = TypeName {unTypeName :: Name} deriving (Eq,Ord,IsString)

-- | Name a Term
newtype TermName = TermName {unTermName :: Name} deriving (Eq,Ord,IsString)

instance Show TypeName where show (TypeName n) = n
instance Show TermName where show (TermName n) = n

