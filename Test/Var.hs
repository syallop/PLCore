module Var where

import Test.QuickCheck

import PL.Var

import Control.Applicative

instance Arbitrary Var where
  arbitrary = (mkVar . fromIntegral) <$> arbitrarySizedNatural

