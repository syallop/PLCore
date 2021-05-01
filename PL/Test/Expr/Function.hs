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

import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.TypeCtx
import PL.TypeCheck
import PL.Var

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
--
-- forall a::k. a -> a
idExprTestCase
  :: Source
  -> ExprTestCase
idExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ)

      , _underResolveCtx = undefined
      , _resolvesTo      = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ)

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo         = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ)
      , _reducesToWhenApplied = []

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = emptyTypeCtx

-- forall a::k0. forall b::k1. a -> b -> a
-- const a b = a
constExprTestCase
  :: Source
  -> ExprTestCase
constExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = BigLam Kind -- k0
        $ BigLam Kind -- k1
        $ Lam (TypeBinding . TyVar . VS $ VZ) -- a :: k0
        $ Lam (TypeBinding . TyVar $ VZ)      -- b :: k1
        $ Binding $ VS VZ                               -- a

      , _underResolveCtx = undefined
      , _resolvesTo      = BigLam Kind -- k0
        $ BigLam Kind -- k1
        $ Lam (TypeBinding . TyVar . VS $ VZ) -- a :: k0
        $ Lam (TypeBinding . TyVar $ VZ)      -- b :: k1
        $ Binding $ VS VZ                               -- a

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = BigArrow Kind -- k0
          $ BigArrow Kind -- k1
          $ Arrow (TypeBinding . TyVar . VS $ VZ) -- a
          $ Arrow (TypeBinding $ TyVar VZ)        -- b
          $ TypeBinding . TyVar $ VS VZ                     -- a

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = BigLam Kind -- k0
          $ BigLam Kind -- k1
          $ Lam (TypeBinding . TyVar . VS $ VZ) -- a :: k0
          $ Lam (TypeBinding . TyVar $ VZ)      -- b :: k1
          $ Binding $ VS VZ                               -- a
      , _reducesToWhenApplied = []

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = emptyTypeCtx

applyExprTestCase
  :: Source
  -> ExprTestCase
applyExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = BigLam Kind $ BigLam Kind
          $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
          $ Lam (TypeBinding . TyVar . VS $ VZ)
          $ App (Binding . VS $ VZ) (Binding VZ)

      , _underResolveCtx = undefined
      , _resolvesTo      = BigLam Kind $ BigLam Kind
          $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
          $ Lam (TypeBinding . TyVar . VS $ VZ)
          $ App (Binding . VS $ VZ) (Binding VZ)

      , _underTypeCheckCtx = topTypeCheckCtx ctx
        -- forall k0 k1. \(f::k0 -> k1) (a::k0) -> f a:: k1
      , _typed = BigArrow Kind $ BigArrow Kind
          $ Arrow (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
          $ Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ)

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = BigLam Kind $ BigLam Kind
          $ Lam (Arrow (TypeBinding . TyVar . VS $ VZ) (TypeBinding . TyVar $ VZ))
          $ Lam (TypeBinding . TyVar . VS $ VZ)
          $ App (Binding . VS $ VZ) (Binding VZ)
      , _reducesToWhenApplied = []

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = emptyTypeCtx

