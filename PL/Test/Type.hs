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

  -- Misc
  , typeCtx
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
-- TODO: Add more type tests here

import PL.Test.Expr.Boolean (boolTypeCtx)
import PL.Test.Expr.Natural (natTypeCtx)
import PL.Test.Expr.List    (listTypeCtx)

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
  { _namedTestCases :: TestNamedSources
  }

-- | Given a collection of test sources, we can produce a list mapping their names
-- to their defined testcases.
mkTypeTestCases
  :: TestTypeSources
  -> Map.Map Text.Text TypeTestCase
mkTypeTestCases t = Map.fromList . mconcat $
  [ namedTestCases . _namedTestCases $ t
  ]

-- | A test type context contains bools, nats and lists.
typeCtx :: TypeCtx DefaultPhase
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

