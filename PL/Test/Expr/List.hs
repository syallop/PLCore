{-# LANGUAGE OverloadedStrings #-}
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
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe
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

listTypeCtx  = insertRecType "List" listType emptyTypeCtx
listTypeName = Named "List"
listType     = TypeLam Kind $ SumT listSumType
listSumType  = NE.fromList $
                 [ ProductT [] -- : List a
                 , ProductT $ [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
                 ]

emptyTerm :: Expr
emptyTerm = BigLam Kind $ Sum (Product []) 0 listSumType

consTerm :: Expr
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

      , _reducesTo = e
      , _reducesToWhenApplied = reduces
      }
  where
    ctx = fromJust $ listTypeCtx <> natTypeCtx
    e   = App (App (BigApp consTerm natTypeName) zero) (BigApp emptyTerm natTypeName)
    ty  = TypeApp listTypeName natType

    -- TODO
    reduces = []

