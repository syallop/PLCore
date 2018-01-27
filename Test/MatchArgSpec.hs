{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MatchArgSpec
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr's MatchArg
-}
module MatchArgSpec where

-- Abstracts the pattern of testing MatchArg parsing and typechecking.
import MatchArgTestCase

-- Some specific MatchArg tests
import MatchArgSpec.Bind
import MatchArgSpec.Binding
import MatchArgSpec.Product
import MatchArgSpec.Sum
import MatchArgSpec.Union

import ExprSpec.Boolean
import ExprSpec.Natural
import ExprSpec.List

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.PLParser.Parser
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.PLPrinter.Printer
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Test.Hspec

-- type context of bools and nats
typeCtx :: TypeCtx TyVar
typeCtx = foldr (unionTypeCtx . fromJust) emptyTypeCtx
  [ boolTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]
-- A List of named TestCase structures
testCases :: [(Text.Text,MatchArgTestCase)]
testCases = mconcat
  [ bindMatchArgTestCases
  , sumMatchArgTestCases
  , productMatchArgTestCases
  , unionMatchArgTestCases
  , bindingMatchArgTestCases
  ]

spec :: Spec
spec = describe "MatchArg Var TyVar" $ sequence_ [parseTo,hasExpectedResult]

-- Test that Text strings parse to an expected expression
parseTo :: Spec
parseTo = describe "A String parses to the expected MatchArg"
        . mapM_ (\(name,testCase) -> parseToSpec name (_parsesFrom testCase) (_isMatchArg testCase))
        $ testCases


-- Test that MatchArgs bind the expected types or fail predictably.
hasExpectedResult :: Spec
hasExpectedResult = describe "A MatchArg evaluates to the expected result"
                  . mapM_ (\(name,testCase) -> hasExpectedResultSpec (_underTypeCtx         testCase)
                                                                     (_underExprBindCtx     testCase)
                                                                     (_underTypeBindCtx     testCase)
                                                                     (_underTypeBindings    testCase)
                                                                     (_isMatchArg           testCase)
                                                                     (_typed                testCase)
                                                                     (_checkMatchWithResult testCase)
                          )
                  $ testCases

