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

import PL.Binds
import PL.Kind
import PL.ReduceType
import PL.Type
import PL.TypeCheck

import Data.Text (Text)
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
  { _parsesFrom = src
  , _parsesTo   = SumT $ NE.fromList [unitTypeName,natTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo      = SumT $ NE.fromList [unitTypeName,natTypeName]

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo            = SumT $ NE.fromList [unitTypeName,natTypeName]
  , _reducesToWhenApplied =
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
  { _parsesFrom = src
  , _parsesTo   = SumT $ NE.fromList [unitTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo      = SumT $ NE.fromList [unitTypeName]

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo            = SumT $ NE.fromList [unitTypeName]
  , _reducesToWhenApplied =
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
  { _parsesFrom = src
  , _parsesTo   = SumT $ NE.fromList [unitTypeName,unitTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo      = SumT $ NE.fromList [unitTypeName,unitTypeName]

  , _underTypeCheckCtx     = topTypeCheckCtx sharedTypeCtx
  , _hasKind              = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = SumT $ NE.fromList [unitTypeName,unitTypeName]
  , _reducesToWhenApplied =
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

