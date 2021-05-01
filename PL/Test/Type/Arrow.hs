{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Arrow
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Arrow' constructor.
-}
module PL.Test.Type.Arrow
  ( arrowTestCases
  , TestArrowSources (..)
  )
  where

import PL.Binds
import PL.Kind
import PL.ReduceType
import PL.Type
import PL.TypeCheck

import Data.Text (Text)

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestArrowSources tests.
data TestArrowSources = TestArrowSources
  { _simpleArrowTestCase :: Source
  }

arrowTestCases
  :: TestArrowSources
  -> [(Text,TypeTestCase)]
arrowTestCases t =
  [ ("Simple Arrow", simpleArrowTestCase . _simpleArrowTestCase $ t)
  ]

simpleArrowTestCase
  :: Source
  -> TypeTestCase
simpleArrowTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = Arrow unitTypeName unitTypeName

  , _underResolveCtx = undefined
  , _resolvesTo      = Arrow unitTypeName unitTypeName

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo            = Arrow unitTypeName unitTypeName
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Can be nested in the first argument"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`Arrow` boolTypeName)
              ]
          , _typeReductionMatches =
              [TypeEquals $ Arrow (Arrow unitTypeName unitTypeName) boolTypeName
              ,TypeDoesNotEqual $ Arrow unitTypeName (Arrow unitTypeName boolTypeName)
              ]
          }

      , TypeReductionTestCase
          { _typeReductionName = "Can be nested in the second argument"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (boolTypeName `Arrow`)
              ]
          , _typeReductionMatches =
              [TypeEquals $ Arrow boolTypeName (Arrow unitTypeName unitTypeName)
              ,TypeDoesNotEqual $ Arrow (Arrow boolTypeName unitTypeName) unitTypeName
              ]
          }
      ]
  }

