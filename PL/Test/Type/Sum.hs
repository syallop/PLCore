{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Sum
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Sum' constructor.
-}
module PL.Test.Type.Sum
  ( sumTestCases
  , TestSumSources (..)
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
import PL.Var

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestSumSources tests.
data TestSumSources = TestSumSources
  { _sumTwoTestCase :: Source
  , _singletonSumTestCase :: Source
  , _duplicateSumTestCase :: Source
  }

sumTestCases
  :: TestSumSources
  -> [(Text,TypeTestCase)]
sumTestCases t =
  [ ("Sum of two", sumTwoTestCase . _sumTwoTestCase $ t)
  , ("Singleton sum", singletonSumTestCase . _singletonSumTestCase $ t)
  , ("Allow multiple occurances of a type", duplicateSumTestCase . _duplicateSumTestCase $ t)
  ]

sumTwoTestCase
  :: Source
  -> TypeTestCase
sumTwoTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  ,_underTypeCheckCtx     = topTypeCheckCtx sharedTypeCtx
  ,_isType               = SumT $ NE.fromList [unitTypeName,natTypeName]
  ,_parsesFrom           = src
  ,_hasKind              = Kind
  ,_reducesTo            = SumT $ NE.fromList [unitTypeName,natTypeName]
  ,_reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Is NOT the same as the reverse sum"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeDoesNotEqual $ SumT $ NE.fromList [natTypeName,unitTypeName]
              ]
          }
      ]
  }

singletonSumTestCase
  :: Source
  -> TypeTestCase
singletonSumTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  ,_underTypeCheckCtx     = topTypeCheckCtx sharedTypeCtx
  ,_isType               = SumT $ NE.fromList [unitTypeName]
  ,_parsesFrom           = src
  ,_hasKind              = Kind
  ,_reducesTo            = SumT $ NE.fromList [unitTypeName]
  ,_reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Sum(Unit) != Sum(Unit,Unit)"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              []
          , _typeReductionMatches =
              [TypeDoesNotEqual $ SumT $ NE.fromList [unitTypeName,unitTypeName]]
          }
     ]
  }

duplicateSumTestCase
  :: Source
  -> TypeTestCase
duplicateSumTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  ,_underTypeCheckCtx     = topTypeCheckCtx sharedTypeCtx
  ,_isType               = SumT $ NE.fromList [unitTypeName,unitTypeName]
  ,_parsesFrom           = src
  ,_hasKind              = Kind
  ,_reducesTo            = SumT $ NE.fromList [unitTypeName,unitTypeName]
  ,_reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Sum(Unit,Unit) != Sum(Unit)"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [TypeDoesNotEqual $ SumT $ NE.fromList [unitTypeName]]
          }
      ]
  }
