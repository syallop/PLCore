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
  , typeCtx
  )
  where

-- Abstracts the pattern of testing expressions parsing, reducing and typechecking
import PL.Test.ExprTestCase

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

-- Some specific ExprSpec tests
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.List
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union

import PL.Test.Shared

import PL.Test.Parsing.Expr
import PL.Test.TypeChecking.Expr
import PL.Test.Reducing.Expr

import PLPrinter

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Test.Hspec
import PL.Test.Source

-- | A record of the sources required to run all of the Expr tests.
data TestExprSources = TestExprSources
  { _lamTestCases      :: TestLamSources
  , _bigLamTestCases   :: TestBigLamSources
  , _booleanTestCases  :: TestBooleanSources
  , _naturalTestCases  :: TestNaturalSources
  , _sumTestCases      :: TestSumSources
  , _productTestCases  :: TestProductSources
  , _unionTestCases    :: TestUnionSources
  , _functionTestCases :: TestFunctionSources
  , _listTestCases     :: TestListSources
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
  , naturalTestCases  . _naturalTestCases  $ t
  , sumTestCases      . _sumTestCases      $ t
  , productTestCases  . _productTestCases  $ t
  , unionTestCases    . _unionTestCases    $ t
  , functionTestCases . _functionTestCases $ t
  , listTestCases     . _listTestCases     $ t
  ]

-- | A test type context contains bools, nats and lists.
typeCtx :: TypeCtx DefaultPhase
typeCtx = foldr unionTypeCtx emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

