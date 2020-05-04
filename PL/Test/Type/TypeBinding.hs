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
  { _simpleTypeBindingTestCase :: Source
  }

typeBindingTestCases
  :: TestTypeBindingSources
  -> [(Text,TypeTestCase)]
typeBindingTestCases t =
  [ ("Simple", simpleTypeBindingTestCase . _simpleTypeBindingTestCase $ t)
  ]

simpleTypeBindingTestCase
  :: Source
  -> TypeTestCase
simpleTypeBindingTestCase src
  = TypeTestCase
  {_underTypeCtx         = ctx
  ,_underTypeBindCtx     = bindCtx
  ,_underBindings        = bindings
  ,_isType               = ty
  ,_parsesFrom           = src
  ,_hasKind              = k
  ,_reducesTo            = stripTypeComments ty
  ,_reducesToWhenApplied = reduces
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
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionUnderTypeBindings = unbound $ emptyBindings
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ TypeBinding $ TyVar $ VZ
              ]
          }
      , TypeReductionTestCase
          {_typeReductionName = "Bindings reduce when bound"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ emptyCtx
          , _typeReductionUnderTypeBindings = bind EmptyProductT $ emptyBindings
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ EmptyProductT
              ]
          }
      , TypeReductionTestCase
          {_typeReductionName = "Bindings reduce recursively"
          , _typeReductionUnderTypeCtx = ctx
          , _typeReductionUnderTypeBindCtx = addBinding Kind $ addBinding Kind $ emptyCtx
          , _typeReductionUnderTypeBindings = bind (TypeBinding $ TyVar $ VS VZ) $ bind (EmptyProductT) $ emptyBindings
          , _typeReductionMutateType =
              [
              ]
          , _typeReductionMatches =
              [ TypeEquals $ EmptyProductT
              ]
          }
      ]

