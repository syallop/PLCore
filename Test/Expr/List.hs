{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.Expr.List
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a heterogenous 'List' type parameterised by some
element type.
-}
module Test.Expr.List
  ( listTypeCtx
  , listTypeName
  , listType
  , listSumType
  , emptyTerm
  , consTerm

  , listNatExprTestCase
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe

import Test.Expr.Natural
import Test.ExprTestCase
import Test.Source

listTypeCtx  = insertRecType "List" listType emptyTypeCtx
listTypeName = fixType $ Named "List"
listType     = fixType $ TypeLam Kind $ fixType $ SumT listSumType
listSumType  = map fixType
                 [ ProductT [] -- : List a
                 , ProductT $ map fixType $ [TypeBinding $ TyVar VZ, TypeApp listTypeName (fixType $ TypeBinding $ TyVar VZ)]
                 ]

emptyTerm :: Expr Var (Type TyVar) TyVar
emptyTerm = fixExpr $ BigLam Kind $ fixExpr $ Sum (fixExpr $ Product []) 0 listSumType

consTerm :: Expr Var (Type TyVar) TyVar
consTerm = fixExpr $ BigLam Kind $ fixExpr $ Lam (fixType $ TypeBinding $ TyVar VZ) $ fixExpr $ Lam (fixType $ TypeApp listTypeName (fixType $ TypeBinding $ TyVar VZ)) $ fixExpr $ Sum (fixExpr $ Product [fixExpr $ Binding $ VS VZ, fixExpr $ Binding VZ]) 1 listSumType

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
      }
  where
    ctx = fromJust $ listTypeCtx <> natTypeCtx
    e   = fixExpr $ App (fixExpr $ App (fixExpr $ BigApp consTerm natTypeName) zero) (fixExpr $ BigApp emptyTerm natTypeName)
    ty  = fixType $ TypeApp listTypeName natType
    {-src = undefined-}

