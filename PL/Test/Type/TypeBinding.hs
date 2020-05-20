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
  {_underTypeReductionCtx = (topTypeReductionCtx ctx){_typeReductionTypeBindings = bindings}
  ,_underTypeBindCtx      = bindCtx
  ,_isType                = ty
  ,_parsesFrom            = src
  ,_hasKind               = k
  ,_reducesTo             = stripTypeComments ty
  ,_reducesToWhenApplied  = reduces
  }
  where
    ctx = sharedTypeCtx
    bindCtx = addBinding Kind $ emptyCtx
    bindings = unbound $ emptyBindings

    ty  = TypeBinding $ TyVar $ VZ
    k = Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Bindings don't reduce when unbound"
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx ctx){_typeReductionTypeBindings = unbound $ emptyBindings}
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
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx ctx){_typeReductionTypeBindings = bind EmptyProductT $ emptyBindings}
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
          , _typeReductionUnderTypeReductionCtx = (topTypeReductionCtx ctx){_typeReductionTypeBindings = bind (TypeBinding $ TyVar $ VS VZ) $ bind (EmptyProductT) $ emptyBindings }
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ addBinding Kind $ emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ EmptyProductT
              ]
          }
      ]

buriedTypeBindingTestCase
  :: Source
  -> TypeTestCase
buriedTypeBindingTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_underTypeBindCtx     = bindCtx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    bindCtx = emptyCtx
    bindings = emptyBindings

    -- \t. ( (\(t->t). (\t. 1)) (\t. 1)
    --  |      |____________|        |
    --  -----------------------------
    --
    -- Should reduce:
    --
    -- \t. \t. \t. 2
    ty  = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ))
                                 (TypeLam Kind $ TypeBinding $ TyVar $ VS VZ)

    k = KindArrow Kind $ KindArrow Kind $ KindArrow Kind Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Type Bindings are adjusted correctly when buried under type lambdas"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
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
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
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

-- Test that burried bindings are incremented by more than 1 where necessary
doubleBuriedTypeBindingTestCase
  :: Source
  -> TypeTestCase
doubleBuriedTypeBindingTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_underTypeBindCtx     = bindCtx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    bindCtx = emptyCtx
    bindings = emptyBindings

    -- \t. ( (\(t->t). (\t. (\t. 2))) (\t. 1)
    -- |       |_________________|         |
    -- ------------------------------------
    --
    -- Should reduce
    --
    -- \t. \t. \t. t. 3
    ty = TypeLam Kind $ TypeApp (TypeLam (KindArrow Kind Kind) (TypeLam Kind $ TypeLam Kind $ TypeBinding $ TyVar $ VS $ VS VZ))
                                 (TypeLam Kind (TypeBinding $ TyVar $ VS VZ))

    k = KindArrow Kind (KindArrow Kind (KindArrow Kind (KindArrow Kind Kind)))

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Type bindings are adjusted correctly when buried under type lambdas"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
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
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
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

-- Test that burried bindings are incremented by more than 1 where necessary
buriedTypeBindingsDontMoveTestCase
  :: Source
  -> TypeTestCase
buriedTypeBindingsDontMoveTestCase src
  = TypeTestCase
  {_underTypeReductionCtx = topTypeReductionCtx ctx
  ,_underTypeBindCtx     = bindCtx
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
  }
  where
    ctx = sharedTypeCtx
    bindCtx = emptyCtx
    bindings = emptyBindings

    ty = TypeLam Kind $ TypeApp (TypeLam Kind (TypeBinding $ TyVar VZ))
                                (TypeBinding $ TyVar VZ)

    k = KindArrow Kind Kind

    reduces =
      [ TypeReductionTestCase
          { _typeReductionName = "Type bindings are not adjusted when they don't move"
          , _typeReductionUnderTypeReductionCtx = topTypeReductionCtx ctx
          , _typeReductionUnderTypeBindCtx = emptyCtx
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeLam Kind $ TypeBinding $ TyVar $ VZ
              ]
          }
      ]

