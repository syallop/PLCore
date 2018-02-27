{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Function
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module ExprSpec.Function
  ( idExprTestCase
  , constExprTestCase
  , applyExprTestCase
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Text (Text)

import ExprTestCase

-- The polymorphic identity function
idExprTestCase :: ExprTestCase
idExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = emptyTypeCtx

    -- forall a::k. a -> a
    e   = fixExpr $ BigLam Kind $ fixExpr $ Lam (TypeBinding $ TyVar VZ) (fixExpr $ Binding VZ) -- \(x:a) -> x
    ty  = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)
    txt = "Λ KIND λ?0 (0)"

constExprTestCase :: ExprTestCase
constExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = emptyTypeCtx
    e   = fixExpr $ BigLam Kind $ fixExpr $ BigLam Kind $ fixExpr $ Lam (TypeBinding . TyVar . VS $ VZ) $ fixExpr $ Lam (TypeBinding . TyVar $ VZ) $ fixExpr $ Binding $ VS VZ -- \(x:a) (y:b) -> x
    ty  = BigArrow Kind $ BigArrow Kind $ Arrow (TypeBinding . TyVar . VS $ VZ) $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding . TyVar . VS $ VZ)
    txt = "Λ KIND KIND λ?1 ?0 (1)"

applyExprTestCase :: ExprTestCase
applyExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = emptyTypeCtx

    e   = fixExpr
        $ BigLam Kind $ fixExpr $BigLam Kind
        $ fixExpr
        $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ fixExpr
        $ Lam (TypeBinding . TyVar . VS $ VZ)
        $ fixExpr
        $ App (fixExpr . Binding . VS $ VZ) (fixExpr $ Binding VZ)

    -- forall k0 k1. \(f::k0 -> k1) (a::k0) -> f a:: k1
    ty  = BigArrow Kind $ BigArrow Kind
        $ Arrow (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ)

    txt = "Λ KIND KIND λ(→ ?1 ?0) ?1 (@1 (0))"

