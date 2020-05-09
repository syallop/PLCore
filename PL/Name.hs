{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , OverloadedStrings
  #-}
{-|
Module      : PL.Name
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Assign new name types to things we really wouldnt want to accidentally confuse.
Type and variable names for example.
-}
module PL.Name
  ( Name

  , TypeName ()
  , typeName
  , mkTypeName

  , TermName ()
  , termName
  , mkTermName
  )
  where

import Data.Monoid
import Data.Text (Text)
import Data.Char
import Data.Maybe
import qualified Data.Text as Text
import GHC.Exts (IsString(..))

import PLPrinter

-- | Name a thing
type Name = Text

-- | Name a Type
newtype TypeName = TypeName {typeName :: Name} deriving (Eq,Ord)

instance IsString TypeName where
  fromString = fromMaybe (error "Invalid TypeName") . mkTypeName . Text.pack

-- | Construct a TypeName that must:
-- - Begin with an Upper case character
-- - Only contain alphabetical characters
-- - Not be a reserved word (such as U)
mkTypeName
  :: Text
  -> Maybe TypeName
mkTypeName txt = case Text.uncons txt of
  Nothing
    -> Nothing
  Just (c,cs)
    | or [ (c == 'U' && cs == "")
         , isLower c
         , Text.any (\c -> not . elem c $ alpha) txt
         ]
      -> Nothing

    | otherwise
      -> Just $ TypeName txt
  where
    alpha = ['a'..'z'] <> ['A'..'Z']

-- | Name a Term
newtype TermName = TermName {termName :: Name} deriving (Eq,Ord)

instance IsString TermName where
  fromString = fromMaybe (error "Invalid TermName") . mkTermName . Text.pack

-- | Construct a TermName that must:
-- - Begin with an Upper case character
-- - Only contain alphabetical characters
-- - Not be a reserved word (such as U)
mkTermName
  :: Text
  -> Maybe TermName
mkTermName txt = case Text.uncons txt of
  Nothing
    -> Nothing
  Just (c,cs)
    | or [ (c == 'U' && cs == "")
         , isLower c
         , Text.any (\c -> not . elem c $ alpha) txt
         ]
      -> Nothing

    | otherwise
      -> Just $ TermName txt
  where
    alpha = ['a'..'z'] <> ['A'..'Z']

instance Show TypeName where show (TypeName n) = Text.unpack n
instance Show TermName where show (TermName n) = Text.unpack n

instance Document TypeName where document (TypeName n) = text n
instance Document TermName where document (TermName n) = text n

