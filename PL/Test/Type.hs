{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Test.Type
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type
-}
module PL.Test.Type
  (
  -- Construct test case input
    TestTypeSources (..)
  , mkTypeTestCases

  -- Test Parsing
  , parsesToTypesSpec
  , parseToTypeSpec

  -- Test Type(kind) checking
  , typeChecksTypesSpec
  , typeCheckTypeSpec

  -- Test Reduction
  , reducesTypesToSpec
  , reduceTypeToSpec
  )
  where

import PL.Test.TypeTestCase

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
import PL.Test.Type.Named
import PL.Test.Type.Arrow
import PL.Test.Type.Sum
import PL.Test.Type.Product
import PL.Test.Type.Union
import PL.Test.Type.TypeBinding
--import PL.Test.Type.BigArrow
--import PL.Test.Type.TypeLam
-- TODO: Add more type tests here

import PL.Test.Shared

import PL.Test.Parsing.Type
import PL.Test.Reducing.Type
import PL.Test.TypeChecking.Type

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

-- | A record of the sources required to run all of the Type tests.
-- TODO: Define some type level tests
data TestTypeSources = TestTypeSources
  { _namedTestCases       :: TestNamedSources
  , _arrowTestCases       :: TestArrowSources
  , _sumTestCases         :: TestSumSources
  , _productTestCases     :: TestProductSources
  , _unionTestCases       :: TestUnionSources
  , _typeBindingTestCases :: TestTypeBindingSources
  }

-- | Given a collection of test sources, we can produce a list mapping their names
-- to their defined testcases.
mkTypeTestCases
  :: TestTypeSources
  -> Map.Map Text.Text TypeTestCase
mkTypeTestCases t = Map.fromList . mconcat $
  [ namedTestCases       . _namedTestCases       $ t
  , arrowTestCases       . _arrowTestCases       $ t
  , sumTestCases         . _sumTestCases         $ t
  , productTestCases     . _productTestCases     $ t
  , unionTestCases       . _unionTestCases       $ t
  , typeBindingTestCases . _typeBindingTestCases $ t
  ]

