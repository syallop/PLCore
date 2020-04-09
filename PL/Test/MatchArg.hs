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
module PL.Test.MatchArg where

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
import PL.Test.Source

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
  -> (TestType -> Doc)
  -> (TestMatchArg -> Doc)
  -> Spec
parserSpec sources testMatchArgP ppType ppMatchArg
  = describe "MatchArg Var TyVar" $ sequence_
    [ parseTo sources testMatchArgP ppMatchArg
    , hasExpectedResult sources ppType
    ]

-- Test that Text strings parse to an expected expression
parseTo
  :: TestMatchArgSources
  -> Parser TestMatchArg
  -> (TestMatchArg -> Doc)
  -> Spec
parseTo sources testMatchArgP ppMatchArg
  = describe "A String parses to the expected MatchArg"
  . mapM_ (\(name,testCase) -> parseToSpec testMatchArgP
                                           name
                                           (_parsesFrom testCase)
                                           (_isMatchArg testCase)
                                           ppMatchArg
          )
  . testCases
  $ sources


-- Test that MatchArgs bind the expected types or fail predictably.
hasExpectedResult
  :: TestMatchArgSources
  -> (TestType -> Doc)
  -> Spec
hasExpectedResult sources ppType
  = describe "A MatchArg evaluates to the expected result"
  . mapM_ (\(name,testCase) -> hasExpectedResultSpec (_underTypeCtx         testCase)
                                                     (_underExprBindCtx     testCase)
                                                     (_underTypeBindCtx     testCase)
                                                     (_underTypeBindings    testCase)
                                                     (_isMatchArg           testCase)
                                                     (_typed                testCase)
                                                     (_checkMatchWithResult testCase)
                                                     ppType
          )
          . testCases
          $ sources

