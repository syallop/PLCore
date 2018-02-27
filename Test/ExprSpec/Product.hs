{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Product
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'product' type.
-}
module ExprSpec.Product
  ( productThreeExprTestCase
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
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))

import ExprSpec.Natural
import ExprSpec.Boolean

import ExprTestCase

productThreeExprTestCase :: ExprTestCase
productThreeExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust $ natTypeCtx <> boolTypeCtx
    e   = fixExpr $ Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ fixExpr $ -- \x : Nat*Bool*Nat ->
      CaseAnalysis $ Case (fixExpr $ Binding VZ)                                      -- case x of
        $ CaseBranches                                                                --
          (CaseBranch (MatchProduct [zPat,Bind,zPat]) (fixExpr $ Binding VZ)          -- Z,y,Z -> y
           :| [CaseBranch (MatchProduct [Bind,Bind,zPat]) (fixExpr $ Binding VZ)]     -- x,y,Z -> y
          )                                                                           --
          (Just                                                                       --
              falseTerm                                                               -- _ -> False
          )
    ty = Arrow (ProductT [natTypeName,boolTypeName,natTypeName]) boolTypeName
    txt = Text.unlines
      ["λ(* Nat Bool Nat) (CASE 0"
      ,"                    (| (* (+0 (*)) (?) (+0 (*))) (0))"
      ,"                    (| (* (?)      (?) (+0 (*))) (0))"
      ,""
      ,"                    (+0 (*) (*) (*))"
      ,"                  )"
      ]

