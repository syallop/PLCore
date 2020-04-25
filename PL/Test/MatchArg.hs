{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Test.MatchArg
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr's MatchArg
-}
module PL.Test.MatchArg
  (
  -- Construct test case input
    TestMatchArgSources (..)
  , mkMatchArgTestCases

  -- Test parsing
  , parsesToMatchArgsSpec
  , parseToMatchArgSpec

  -- Test Type checking
  , typeChecksMatchArgsSpec
  , typeCheckMatchArgSpec

  -- Test Reduction
  , reducesMatchArgsToSpec
  , reduceMatchArgToSpec

  -- Misc
  , typeCtx
  )
  where

-- Abstracts the pattern of testing MatchArg parsing and typechecking.
import PL.Test.MatchArgTestCase

-- Some specific MatchArg tests
import PL.Test.MatchArg.Bind
import PL.Test.MatchArg.Binding
import PL.Test.MatchArg.Product
import PL.Test.MatchArg.Sum
import PL.Test.MatchArg.Union

import PL.Test.Expr.Boolean
import PL.Test.Expr.Natural
import PL.Test.Expr.List

import PL.Test.Parsing.MatchArg
import PL.Test.TypeChecking.MatchArg
import PL.Test.Reducing.MatchArg

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

import PLParser
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

-- | A record of the sources required to run all of the MatchArg tests.
data TestMatchArgSources = TestMatchArgSources
  { _bindTestCases    :: TestBindSources
  , _sumTestCases     :: TestSumSources
  , _productTestCases :: TestProductSources
  , _unionTestCases   :: TestUnionSources
  , _bindingTestCases :: TestBindingSources
  }

-- | Given a collection of test sources, we can produce a list mapping their names
-- to their defined testcases.
mkMatchArgTestCases
  :: TestMatchArgSources
  -> Map.Map Text.Text MatchArgTestCase
mkMatchArgTestCases t = Map.fromList . mconcat $
  [ bindTestCases    . _bindTestCases    $ t
  , sumTestCases     . _sumTestCases     $ t
  , productTestCases . _productTestCases $ t
  , unionTestCases   . _unionTestCases   $ t
  , bindingTestCases . _bindingTestCases $ t
  ]

-- type context of bools and nats
typeCtx :: TypeCtx DefaultPhase
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

