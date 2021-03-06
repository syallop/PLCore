{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.Product
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'Product' constructor.
-}
module PL.Test.Type.Product
  ( productTestCases
  , TestProductSources (..)
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

-- A record of the sources required to run all the TestProductSources tests.
data TestProductSources = TestProductSources
  { _emptyProductTestCase     :: Source
  , _singletonProductTestCase :: Source
  , _twoProductTestCase       :: Source
  , _duplicateProductTestCase :: Source
  }

productTestCases
  :: TestProductSources
  -> [(Text,TypeTestCase)]
productTestCases t =
  [ ("Empty product", emptyProductTestCase . _emptyProductTestCase $ t)
  , ("Singleton product", singletonProductTestCase . _singletonProductTestCase $ t)
  , ("Two product", twoProductTestCase . _twoProductTestCase $ t)
  , ("Duplicate product", duplicateProductTestCase . _duplicateProductTestCase $ t)
  ]

emptyProductTestCase
  :: Source
  -> TypeTestCase
emptyProductTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = ProductT []

  , _underResolveCtx = undefined
  , _resolvesTo      = ProductT []

  , _underTypeCheckCtx     = topTypeCheckCtx sharedTypeCtx
  , _hasKind              = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  ,_reducesTo            = ProductT []
  ,_reducesToWhenApplied = []
  }

singletonProductTestCase
  :: Source
  -> TypeTestCase
singletonProductTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = ProductT [unitTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo      = ProductT [unitTypeName]

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo             = ProductT [unitTypeName]
  , _reducesToWhenApplied  = []
  }

twoProductTestCase
  :: Source
  -> TypeTestCase
twoProductTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = ProductT [unitTypeName,natTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo = ProductT [unitTypeName,natTypeName]

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo             = ProductT [unitTypeName,natTypeName]
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Is not the same as it's reverse"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [TypeDoesNotEqual $ ProductT [natTypeName, unitTypeName]]
          }
      ]
  }

duplicateProductTestCase
  :: Source
  -> TypeTestCase
duplicateProductTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = ProductT [unitTypeName,unitTypeName]

  , _underResolveCtx = undefined
  , _resolvesTo = ProductT [unitTypeName,unitTypeName]

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind           = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo            = ProductT [unitTypeName,unitTypeName]
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Does not lose duplicates"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [TypeDoesNotEqual $ ProductT [unitTypeName]]
          }
      ]
  }

