{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : GrammarSpec
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Grammar.
-}
module GrammarSpec where

import PL.Parser
import PL.Printer
import PL.Grammar
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

spec :: Spec
spec = describe "grammar" $ sequence_
  [
  ]
