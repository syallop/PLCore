{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Test.Pattern
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr's Pattern
-}
module PL.Test.Pattern
  (
  -- Construct test case input
    TestPatternSources (..)
  , mkPatternTestCases

  -- Test parsing
  , parsesToPatternsSpec
  , parseToPatternSpec

  -- Test Type checking
  , typeChecksPatternsSpec
  , typeCheckPatternSpec

  -- Test Reduction
  , reducesPatternsToSpec
  , reducePatternToSpec
  )
  where

-- Abstracts the pattern of testing Pattern parsing and typechecking.
import PL.Test.PatternTestCase

-- Some specific Pattern tests
import PL.Test.Pattern.Bind
import PL.Test.Pattern.Binding
import PL.Test.Pattern.Product
import PL.Test.Pattern.Sum
import PL.Test.Pattern.Union

import PL.Test.Shared

import PL.Test.Parsing.Pattern
import PL.Test.TypeChecking.Pattern
import PL.Test.Reducing.Pattern

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLPrinter

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec
import PL.Test.Source

-- | A record of the sources required to run all of the Pattern tests.
data TestPatternSources = TestPatternSources
  { _bindTestCases    :: TestBindSources
  , _sumTestCases     :: TestSumSources
  , _productTestCases :: TestProductSources
  , _unionTestCases   :: TestUnionSources
  , _bindingTestCases :: TestBindingSources
  }

-- | Given a collection of test sources, we can produce a list mapping their names
-- to their defined testcases.
mkPatternTestCases
  :: TestPatternSources
  -> Map.Map Text.Text PatternTestCase
mkPatternTestCases t = Map.fromList . mconcat $
  [ bindTestCases    . _bindTestCases    $ t
  , sumTestCases     . _sumTestCases     $ t
  , productTestCases . _productTestCases $ t
  , unionTestCases   . _unionTestCases   $ t
  , bindingTestCases . _bindingTestCases $ t
  ]

