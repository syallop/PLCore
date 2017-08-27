{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.List
  ( listTypeCtx
  , listTypeName
  , listType
  , listSumType
  , emptyTerm
  , consTerm

  , listNatExprTestCase
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
import Data.Monoid ((<>))
import Data.Maybe

import ExprSpec.Natural
import ExprTestCase

listTypeCtx  = insertRecType "List" listType emptyTypeCtx
listTypeName = Named "List"
listType     = TypeLam Kind $ SumT listSumType
listSumType  = [ProductT [] -- : List a
               ,      ProductT [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
               ]
emptyTerm    = BigLam Kind $ Sum (Product []) 0 listSumType
consTerm     = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $ Sum (Product [Binding $ VS VZ,Binding VZ]) 1 listSumType

-- [0]
listNatExprTestCase :: ExprTestCase
listNatExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust $ listTypeCtx <> natTypeCtx
    e   = App (App (BigApp consTerm natTypeName) zero) (BigApp emptyTerm natTypeName)
    ty  = TypeApp listTypeName natType
    txt = undefined

