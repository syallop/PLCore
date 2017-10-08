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
import PL.Grammar.Lispy hiding (appise,lamise)
import PL.Kind
import PL.Parser
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

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
    e   = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ) -- \(x:a) -> x
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
    e   = BigLam Kind $ BigLam Kind $ Lam (TypeBinding . TyVar . VS $ VZ) $ Lam (TypeBinding . TyVar $ VZ) $ Binding $ VS VZ -- \(x:a) (y:b) -> x
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

    e   = BigLam Kind $ BigLam Kind
        $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ Lam (TypeBinding . TyVar . VS $ VZ)
        $ App (Binding . VS $ VZ) (Binding VZ)

    -- forall k0 k1. \(f::k0 -> k1) (a::k0) -> f a:: k1
    ty  = BigArrow Kind $ BigArrow Kind
        $ Arrow (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ)

    txt = "Λ KIND KIND λ(→ ?1 ?0) ?1 (@1 (0))"

