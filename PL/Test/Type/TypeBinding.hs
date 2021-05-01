{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Type.TypeBinding
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Type using the 'TypeBinding' constructor.
-}
module PL.Test.Type.TypeBinding
  ( typeBindingTestCases
  , TestTypeBindingSources (..)
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

-- A record of the sources required to run all the TestTypeBindingSources tests.
data TestTypeBindingSources = TestTypeBindingSources
  { _simpleTypeBindingTestCase          :: Source
  , _buriedTypeBindingTestCase          :: Source
  , _doubleBuriedTypeBindingTestCase    :: Source
  , _buriedTypeBindingsDontMoveTestCase :: Source
  }

typeBindingTestCases
  :: TestTypeBindingSources
  -> [(Text,TypeTestCase)]
typeBindingTestCases t =
  [ ("Simple type bindings"           , simpleTypeBindingTestCase          . _simpleTypeBindingTestCase          $ t)
  , ("Buried type bindings"           , buriedTypeBindingTestCase          . _buriedTypeBindingTestCase          $ t)
  , ("Double buried type bindings"    , doubleBuriedTypeBindingTestCase    . _doubleBuriedTypeBindingTestCase    $ t)
  , ("Buried type bindings don't move", buriedTypeBindingsDontMoveTestCase . _buriedTypeBindingsDontMoveTestCase $ t)
  ]

simpleTypeBindingTestCase
  :: Source
  -> TypeTestCase
simpleTypeBindingTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = TypeBinding $ TyVar $ VZ

  , _underResolveCtx = undefined
  , _resolvesTo      = TypeBinding $ TyVar $ VZ

  , _underTypeCheckCtx = (topTypeCheckCtx sharedTypeCtx){_typeBindings = unbound emptyBindings, _typeBindCtx = addBinding Kind emptyCtx}
  , _hasKind = Kind

  , _underTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = unbound emptyBindings}
  , _reducesTo = TypeBinding $ TyVar $ VZ
  , _reducesToWhenApplied =
    [ TypeReductionTestCase
          { _typeReductionName = "Bindings don't reduce when unbound"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = unbound $ emptyBindings}
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeBinding $ TyVar $ VZ
              ]
          }
      , TypeReductionTestCase
          {_typeReductionName = "Bindings reduce when bound"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = bind EmptyProductT $ emptyBindings}
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ EmptyProductT
              ]
          }
      , TypeReductionTestCase
          {_typeReductionName = "Bindings reduce recursively"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx sharedTypeCtx){_typeReductionTypeBindings = bind (TypeBinding $ TyVar $ VS VZ) $ bind (EmptyProductT) $ emptyBindings }
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ EmptyProductT
              ]
          }
      ]
  }

-- \t. ( (\(t->t). (\t. 1)) (\t. 1)
--  |      |____________|        |
--  -----------------------------
--
-- Should reduce:
--
-- \t. \t. \t. 2
buriedTypeBindingTestCase
  :: Source
  -> TypeTestCase
buriedTypeBindingTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ))
                                 (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ)


  , _underResolveCtx = undefined
  , _resolvesTo      = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ))
                                 (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ)

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = KindArrow Kind $ KindArrow Kind $ KindArrow Kind Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ))
                                 (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ)
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Type Bindings are adjusted correctly when buried under type lambdas"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS $ VZ -- Note the original binding should have been increased from 1 to 2 as it's application moved its binding further away.
              ]
          }
      , TypeReductionTestCase
          { _typeReductionName = "Type Bindings reduce to the correct value"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [ (`TypeApp` boolTypeName)
              , (`TypeApp` unitTypeName)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind boolTypeName
              ]
          }
      ]
  }

-- Test that burried bindings are incremented by more than 1 where necessary
-- \t. ( (\(t->t). (\t. (\t. 2))) (\t. 1)
-- |       |_________________|         |
-- ------------------------------------
--
-- Should reduce
--
-- \t. \t. \t. t. 3
doubleBuriedTypeBindingTestCase
  :: Source
  -> TypeTestCase
doubleBuriedTypeBindingTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS VZ))
                                 (TypeLam Kind (TypeBinding $ TyVar $ VS VZ))

  , _underResolveCtx = undefined
  , _resolvesTo      = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS VZ))
                                 (TypeLam Kind (TypeBinding $ TyVar $ VS VZ))

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = KindArrow Kind (KindArrow Kind (KindArrow Kind (KindArrow Kind Kind)))

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS VZ))
                                 (TypeLam Kind (TypeBinding $ TyVar $ VS VZ))
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Type bindings are adjusted correctly when buried under type lambdas"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ TypeLam Kind $ TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS $ VS $ VZ
              ]
          }
      ,  TypeReductionTestCase
          { _typeReductionName = "Buried type bindings reduce to the correct value"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [(`TypeApp` boolType)
              ,(`TypeApp` unitType)
              ,(`TypeApp` natType)
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind boolType
              ]
          }
      ]
  }

-- Test that burried bindings are incremented by more than 1 where necessary
buriedTypeBindingsDontMoveTestCase
  :: Source
  -> TypeTestCase
buriedTypeBindingsDontMoveTestCase src
  = TypeTestCase
  { _parsesFrom = src
  , _parsesTo = TypeLam Kind $ TypeApp (TypeLam Kind (TypeBinding $ TyVar VZ))
                                (TypeBinding $ TyVar VZ)

  , _underResolveCtx = undefined
  , _resolvesTo = TypeLam Kind $ TypeApp (TypeLam Kind (TypeBinding $ TyVar VZ))
                                (TypeBinding $ TyVar VZ)

  , _underTypeCheckCtx = topTypeCheckCtx sharedTypeCtx
  , _hasKind = KindArrow Kind Kind

  , _underTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
  , _reducesTo = TypeLam Kind $ TypeApp (TypeLam Kind (TypeBinding $ TyVar VZ))
                                (TypeBinding $ TyVar VZ)
  , _reducesToWhenApplied =
      [ TypeReductionTestCase
          { _typeReductionName = "Type bindings are not adjusted when they don't move"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx sharedTypeCtx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ TypeBinding $ TyVar $ VZ
              ]
          }
      ]
    }

