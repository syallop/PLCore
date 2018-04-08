{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Function
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module PL.Test.Expr.Function
  ( idExprTestCase
  , constExprTestCase
  , applyExprTestCase

  , TestFunctionSources (..)
  , functionTestCases
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

import PL.Test.ExprTestCase
import PL.Test.Source

data TestFunctionSources = TestFunctionSources
  { _idTestCase    :: Source
  , _constTestCase :: Source
  , _applyTestCase :: Source
  }

functionTestCases
  :: TestFunctionSources
  -> [(Text, ExprTestCase)]
functionTestCases t =
  [ ("id"   , idExprTestCase    . _idTestCase    $ t)
  , ("const", constExprTestCase . _constTestCase $ t)
  , ("apply", applyExprTestCase . _applyTestCase $ t)
  ]

-- The polymorphic identity function
idExprTestCase
  :: Source
  -> ExprTestCase
idExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src
      }
  where
    ctx = emptyTypeCtx

    -- forall a::k. a -> a
    e   = fixExpr $ BigLam Kind $ fixExpr $ Lam (fixType $ TypeBinding $ TyVar VZ) (fixExpr $ Binding VZ) -- \(x:a) -> x
    ty  = fixType $ BigArrow Kind $ fixType $ Arrow (fixType $ TypeBinding $ TyVar VZ) (fixType $ TypeBinding $ TyVar VZ)

constExprTestCase
  :: Source
  -> ExprTestCase
constExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src
      }
  where
    ctx = emptyTypeCtx
    e   = fixExpr $ BigLam Kind $ fixExpr $ BigLam Kind $ fixExpr $ Lam (fixType . TypeBinding . TyVar . VS $ VZ) $ fixExpr $ Lam (fixType . TypeBinding . TyVar $ VZ) $ fixExpr $ Binding $ VS VZ -- \(x:a) (y:b) -> x
    ty  = fixType $ BigArrow Kind $ fixType $ BigArrow Kind $ fixType $ Arrow (fixType . TypeBinding . TyVar . VS $ VZ) $ fixType $ Arrow (fixType $ TypeBinding $ TyVar VZ) (fixType . TypeBinding . TyVar . VS $ VZ)

applyExprTestCase
  :: Source
  -> ExprTestCase
applyExprTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src
      }
  where
    ctx = emptyTypeCtx

    e   = fixExpr
        $ BigLam Kind $ fixExpr $ BigLam Kind
        $ fixExpr
        $ Lam (fixType $ Arrow (fixType . TypeBinding . TyVar . VS $ VZ) (fixType . TypeBinding . TyVar $ VZ))
        $ fixExpr
        $ Lam (fixType . TypeBinding . TyVar . VS $ VZ)
        $ fixExpr
        $ App (fixExpr . Binding . VS $ VZ) (fixExpr $ Binding VZ)

    -- forall k0 k1. \(f::k0 -> k1) (a::k0) -> f a:: k1
    ty  = fixType $ BigArrow Kind $ fixType $ BigArrow Kind
        $ fixType $ Arrow (fixType $ Arrow (fixType . TypeBinding . TyVar . VS $ VZ) (fixType . TypeBinding . TyVar $ VZ))
        $ fixType
        $ Arrow (fixType . TypeBinding . TyVar . VS $ VZ) (fixType . TypeBinding . TyVar $ VZ)


