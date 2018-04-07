{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.MatchArg
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr's MatchArg
-}
module Test.MatchArg where

-- Abstracts the pattern of testing MatchArg parsing and typechecking.
import Test.MatchArgTestCase

-- Some specific MatchArg tests
import Test.MatchArg.Bind
import Test.MatchArg.Binding
import Test.MatchArg.Product
import Test.MatchArg.Sum
import Test.MatchArg.Union

import Test.Expr.Boolean
import Test.Expr.Natural
import Test.Expr.List

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

import Test.Hspec
import Test.Source

-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

data TestMatchArgSources = TestMatchArgSources
  { _bindTestCases    :: TestBindSources
  , _sumTestCases     :: TestSumSources
  , _productTestCases :: TestProductSources
  , _unionTestCases   :: TestUnionSources
  , _bindingTestCases :: TestBindingSources
  }

-- A List of named TestCase structures
testCases
  :: TestMatchArgSources
  -> [(Text.Text, MatchArgTestCase)]
testCases t = mconcat
  [ bindTestCases    . _bindTestCases    $ t
  , sumTestCases     . _sumTestCases     $ t
  , productTestCases . _productTestCases $ t
  , unionTestCases   . _unionTestCases   $ t
  , bindingTestCases . _bindingTestCases $ t
  ]

-- Define:
--
-- spec :: Spec
-- spec = parserSpec YOURPARSER
--
-- to define a spec testing your MatchArg parser.
parserSpec
  :: TestMatchArgSources
  -> Parser TestMatchArg
  -> Spec
parserSpec sources testMatchArgP
  = describe "MatchArg Var TyVar" $ sequence_
    [ parseTo sources testMatchArgP
    , hasExpectedResult sources
    ]

-- Test that Text strings parse to an expected expression
parseTo
  :: TestMatchArgSources
  -> Parser TestMatchArg
  -> Spec
parseTo sources testMatchArgP
  = describe "A String parses to the expected MatchArg"
  . mapM_ (\(name,testCase) -> parseToSpec testMatchArgP name (_parsesFrom testCase) (_isMatchArg testCase))
  . testCases
  $ sources


-- Test that MatchArgs bind the expected types or fail predictably.
hasExpectedResult
  :: TestMatchArgSources
  -> Spec
hasExpectedResult sources
  = describe "A MatchArg evaluates to the expected result"
  . mapM_ (\(name,testCase) -> hasExpectedResultSpec (_underTypeCtx         testCase)
                                                     (_underExprBindCtx     testCase)
                                                     (_underTypeBindCtx     testCase)
                                                     (_underTypeBindings    testCase)
                                                     (_isMatchArg           testCase)
                                                     (_typed                testCase)
                                                     (_checkMatchWithResult testCase)
          )
          . testCases
          $ sources

