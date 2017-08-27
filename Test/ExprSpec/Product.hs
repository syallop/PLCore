{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Product
  ( productThreeExprTestCase
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
    e   = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
      CaseAnalysis $ Case (Binding VZ)                                       -- case x of
        $ CaseBranches                                                       --
          ((CaseBranch (MatchProduct [zPat,Bind,zPat]) (Binding VZ))         -- Z,y,Z -> y
           :| [CaseBranch (MatchProduct [Bind,Bind,zPat]) (Binding VZ)]      -- x,y,Z -> y
          )                                                                  --
          (Just                                                              --
              falseTerm                                                      -- _ -> False
          )
    ty = Arrow (ProductT [natTypeName,boolTypeName,natTypeName]) boolTypeName
    txt = Text.unlines
      ["\\(* Nat Bool Nat) (CASE 0"
      ,"                    (| (* (+0 (*)) (?) (+0 (*))) (0))"
      ,"                    (| (* (?)      (?) (+0 (*))) (0))"
      ,""
      ,"                    (+0 (*) (*) (*))"
      ,"                  )"
      ]

