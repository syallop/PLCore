{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Test.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr
-}
module PL.Test.Expr
  (
  -- Construct test case input
    TestExprSources (..)
  , mkTestCases

  -- Test Parsing
  , parsesToSpec
  , parseToSpec

  -- Test Type Checking
  , typeChecksSpec
  , typeCheckSpec

  -- Test Reduction
  , reducesToSpec
  , reduceToSpec

  -- Misc
  , sharedTypeCtx
  )
  where

-- Abstracts the pattern of testing expressions parsing, reducing and typechecking
import PL.Test.ExprTestCase

-- Some specific ExprSpec tests
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Binding
import PL.Test.Expr.Function
import PL.Test.Expr.Maybe
import PL.Test.Expr.Lam
import PL.Test.Expr.List
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union
import PL.Test.Expr.SelfTypes

import PL.Test.Shared

import PL.Test.Parsing.Expr
import PL.Test.TypeChecking.Expr
import PL.Test.Reducing.Expr

import qualified Data.Map as Map
import qualified Data.Text as Text

-- | A record of the sources required to run all of the Expr tests.
data TestExprSources = TestExprSources
  { _lamTestCases      :: TestLamSources
  , _bigLamTestCases   :: TestBigLamSources
  , _booleanTestCases  :: TestBooleanSources
  , _bindingTestCases  :: TestBindingSources
  , _naturalTestCases  :: TestNaturalSources
  , _sumTestCases      :: TestSumSources
  , _productTestCases  :: TestProductSources
  , _unionTestCases    :: TestUnionSources
  , _functionTestCases :: TestFunctionSources
  , _maybeTestCases    :: TestMaybeSources
  , _listTestCases     :: TestListSources
  , _selfTypeTestCases :: TestSelfTypeSources
  }

-- | Given a collection of test sources, we can produce a list mapping their names
-- to their defined testcases.
mkTestCases
  :: TestExprSources
  -> Map.Map Text.Text ExprTestCase
mkTestCases t = Map.fromList . mconcat $
  [ lamTestCases      . _lamTestCases      $ t
  , bigLamTestCases   . _bigLamTestCases   $ t
  , booleanTestCases  . _booleanTestCases  $ t
  , bindingTestCases  . _bindingTestCases  $ t
  , naturalTestCases  . _naturalTestCases  $ t
  , sumTestCases      . _sumTestCases      $ t
  , productTestCases  . _productTestCases  $ t
  , unionTestCases    . _unionTestCases    $ t
  , functionTestCases . _functionTestCases $ t
  , maybeTestCases    . _maybeTestCases    $ t
  , listTestCases     . _listTestCases     $ t
  , selfTypeTestCases . _selfTypeTestCases $ t
  ]

