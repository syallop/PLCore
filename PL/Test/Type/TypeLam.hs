{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.TypeLam
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'TypeLam' constructor.
-}
module PL.Test.Type.TypeLam
  ( typeLamTestCases
  , TestTypeLamSources (..)
  )
  where

import PL.Binds
import PL.Kind
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.TypeCheck
import PL.Var

import Data.Text (Text)

import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Test.Shared

-- A record of the sources required to run all the TestTypeLamSources tests.
data TestTypeLamSources = TestTypeLamSources
  { _simpleTypeLamTestCase :: Source
  , _nestedTypeLamTestCase :: Source
  }

typeLamTestCases
  :: TestTypeLamSources
  -> [(Text,TypeTestCase)]
typeLamTestCases t =
  [ ("Simple Type Lam", simpleTypeLamTestCase . _simpleTypeLamTestCase $ t)
  , ("Nested Type Lam", nestedTypeLamTestCase . _nestedTypeLamTestCase $ t)
  ]

simpleTypeLamTestCase
  :: Source
  -> TypeTestCase
simpleTypeLamTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = TypeLam Kind $ TypeBinding $ TyVar VZ

  , _underResolveCtx = undefined
  , _resolvesTo = TypeLam Kind $ TypeBinding $ TyVar VZ

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = KindArrow Kind Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = TypeLam Kind $ TypeBinding $ TyVar VZ
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Bind types when applied"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` unitTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ unitTypeName
              ]
          }
      ]
  }

nestedTypeLamTestCase
  :: Source
  -> TypeTestCase
nestedTypeLamTestCase src
 = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS VZ

  , _underResolveCtx = undefined
  , _resolvesTo = TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS VZ

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = KindArrow Kind $ KindArrow Kind Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS VZ
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Reduces to outer type"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` boolTypeName)
              , (`TypeApp` natTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ boolTypeName
              ]
          }

      , TypeReductionTestCase
          { _typeReductionName = "Reduces under type lambda"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` boolTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ boolTypeName
              ]
          }
      ]
  }

