{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Function
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using 'function' types.
-}
module ExprSpec.Function
  ( identityTypeName
  , identityType
  , identityTerm

  , idExprTestCase
  , constExprTestCase
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Parser
import PL.Parser.Lispy hiding (appise,lamise)
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

import Data.Text (Text)

import ExprTestCase

identityTypeName = Named "Identity"
identityType     = TypeLam Kind $ ProductT [TypeBinding $ TyVar VZ]
identityTerm     = undefined

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
    e   = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ) -- \(x:a) -> x
    ty  = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)
    txt = undefined

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
    txt = undefined

