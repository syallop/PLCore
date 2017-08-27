{-|
Module      : Var
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Functions used for testing PL.Var
-}
module Var where

import Test.QuickCheck

import PL.Var

import Control.Applicative

instance Arbitrary Var where
  arbitrary = (mkVar . fromIntegral) <$> arbitrarySizedNatural

