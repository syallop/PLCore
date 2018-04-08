{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : PL.Name
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Functions used for testing PL.Name
-}

module PL.Test.Name where

import Test.QuickCheck

import PL.Name

import Data.Text
import Control.Applicative

instance Arbitrary Name where
  arbitrary = (\c cs -> pack (c:cs)) <$> elements ['A' .. 'Z']) <*> listOf (elements ['a' .. 'z'])

instance Arbitrary TypeName where
  arbitrary = TypeName <$> arbitrary

instance Arbitrary TermName where
  arbitrary = TermName <$> arbitrary

