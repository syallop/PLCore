{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Function
  ( identityTypeName
  , identityType
  , identityTerm

  , idExpr
  ,idExprType
  ,constExpr
  ,constExprType
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

type TestType = Type TyVar
type TestExpr = Expr Var TestType TyVar

identityTypeName = Named "Identity"
identityType     = TypeLam Kind $ ProductT [TypeBinding $ TyVar VZ]
identityTerm     = undefined

-- The polymorphic identity function
idExpr :: TestExpr
idExpr = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) (Binding VZ) -- \(x:a) -> x
idExprType :: TestType
idExprType = BigArrow Kind $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar VZ)

constExpr :: TestExpr
constExpr = BigLam Kind $ BigLam Kind $ Lam (TypeBinding $ TyVar $ VS VZ) $ Lam (TypeBinding $ TyVar VZ) $ Binding $ VS VZ -- \(x:a) (y:b) -> x
constExprType :: TestType
constExprType = BigArrow Kind $ BigArrow Kind $ Arrow (TypeBinding $ TyVar $ VS $ VZ) $ Arrow (TypeBinding $ TyVar VZ) (TypeBinding $ TyVar $ VS VZ)

