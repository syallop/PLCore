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
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
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

  -- TODO: Potential memory leak
  , ("const", constExprTestCase . _constTestCase $ t)
  {-, ("apply", applyExprTestCase . _applyTestCase $ t)-}
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
    e   = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ) -- \(x:a) -> x
    ty  = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)

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

    -- forall a::k0. forall b::k1. a -> b -> a
    -- const a b = a
    e   = BigLam Kind -- k0
        $ BigLam Kind -- k1
        $ Lam (TypeBinding . TyVar . VS $ VZ) -- a :: k0
        $ Lam (TypeBinding . TyVar $ VZ)      -- b :: k1
        $ Binding $ VS VZ                               -- a

    ty  = BigArrow Kind -- k0
        $ BigArrow Kind -- k1
        $ Arrow (TypeBinding . TyVar . VS $ VZ) -- a
        $ Arrow (TypeBinding $ TyVar VZ)        -- b
        $ TypeBinding . TyVar $ VS VZ                     -- a

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

    e   = BigLam Kind $ BigLam Kind
        $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ Lam (TypeBinding . TyVar . VS $ VZ)
        $ App (Binding . VS $ VZ) (Binding VZ)

    -- forall k0 k1. \(f::k0 -> k1) (a::k0) -> f a:: k1
    ty  = BigArrow Kind $ BigArrow Kind
        $ Arrow (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
        $ Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ)


