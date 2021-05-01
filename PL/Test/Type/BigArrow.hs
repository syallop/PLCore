{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.BigArrow
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'BigArrow' constructor.
-}
module PL.Test.Type.BigArrow
  ( bigArrowTestCases
  , TestBigArrowSources (..)
  )
  where

import PL.Bindings
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

-- A record of the sources required to run all the TestBigArrowSources tests.
data TestBigArrowSources = TestBigArrowSources
  { _simpleBigArrowTestCase  :: Source
  , _complexBigArrowTestCase :: Source
  }

bigArrowTestCases
  :: TestBigArrowSources
  -> [(Text,TypeTestCase)]
bigArrowTestCases t =
  [ ("Simple Big Arrow", simpleBigArrowTestCase . _simpleBigArrowTestCase $ t)
  , ("Complex Big Arrow", complexBigArrowTestCase . _complexBigArrowTestCase $ t)
  ]

simpleBigArrowTestCase
  :: Source
  -> TypeTestCase
simpleBigArrowTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = BigArrow Kind unitTypeName

  , _underResolveCtx = undefined
  , _resolvesTo = BigArrow Kind unitTypeName

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = BigArrow Kind unitTypeName
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Can be nested in the second argument"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (Kind `BigArrow`)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ BigArrow Kind (BigArrow Kind unitTypeName)
              ]
          }
      ]
  }

-- Testing that types that have entered the bindctx from the expression level
-- are resolved.
complexBigArrowTestCase
  :: Source
  -> TypeTestCase
complexBigArrowTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo   = BigArrow Kind (TypeBinding $ TyVar $ VZ)

  , _underResolveCtx = undefined
  , _resolvesTo      = BigArrow Kind (TypeBinding $ TyVar $ VZ)

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = Kind

  , _underTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = unbound emptyBindings}
  , _reducesTo = BigArrow Kind (TypeBinding $ TyVar $ VZ)
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Unbound types allowed"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = unbound emptyBindings}
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ BigArrow Kind (TypeBinding $ TyVar $ VZ)
              ]
          }

     , TypeReductionTestCase
          { _typeReductionName = "Bound types allowed"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = bind unitTypeName emptyBindings}
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ BigArrow Kind unitTypeName
              ]
          }

      ]
  }
