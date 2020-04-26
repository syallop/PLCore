{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : PL.Test.Expr.List
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a heterogenous 'List' type parameterised by some
element type.
-}
module PL.Test.Expr.List
  ( listTypeCtx
  , listTypeName
  , listType
  , listSumType
  , emptyTerm
  , consTerm

  , listNatExprTestCase

  , TestListSources (..)
  , listTestCases
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

import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe
import Data.List.NonEmpty (NonEmpty) 
import qualified Data.List.NonEmpty as NE

import PL.Test.Expr.Natural
import PL.Test.ExprTestCase
import PL.Test.Source

data TestListSources = TestListSources
  { _listTestCase :: Source
  }

listTestCases
  :: TestListSources
  -> [(Text, ExprTestCase)]
listTestCases t =
  [
  ]

listTypeCtx
  :: (TypeLamExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => TypeCtx phase
listTypeCtx = fromJust $ insertRecType "List" listType emptyTypeCtx

listTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
listTypeName = Named "List"

listType
  :: (TypeLamExtension     phase ~ Void
     ,SumTExtension        phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => TypeFor phase
listType = TypeLam Kind $ SumT listSumType

listSumType
  :: (ProductTExtension    phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => NonEmpty (TypeFor phase)
listSumType = NE.fromList $
 [ ProductT [] -- : List a
 , ProductT $ [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
 ]

emptyTerm
  :: (BigLamExtension      phase ~ Void
     ,SumExtension         phase ~ Void
     ,ProductExtension     phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => ExprFor phase
emptyTerm = BigLam Kind $ Sum (Product []) 0 listSumType

consTerm
  :: forall phase
   . (BigLamExtension      phase ~ Void
     ,SumExtension         phase ~ Void
     ,ProductExtension     phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,NamedExtension       phase ~ Void
     ,LamExtension         phase ~ Void
     ,AbstractionFor       phase ~ TypeFor phase
     ,BindingExtension     phase ~ Void
     ,BindingFor           phase ~ Var
     )
  => ExprFor phase
consTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $ Sum (Product [Binding $ VS VZ, Binding VZ]) 1 listSumType

-- [0]
listNatExprTestCase
  :: Source
  -> ExprTestCase
listNatExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      , _reducesTo = stripComments e
      , _reducesToWhenApplied = reduces
      }
  where
    ctx = listTypeCtx <> natTypeCtx
    e   = App (App (BigApp consTerm natTypeName) zero) (BigApp emptyTerm natTypeName)
    ty  = TypeApp listTypeName natType

    -- TODO
    reduces = []

