{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Product
  ( productThreeExpr
  , productThreeExprType
  , productThreeText
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
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))

import ExprSpec.Natural
import ExprSpec.Boolean

type TestType = Type TyVar
type TestExpr = Expr Var TestType TyVar

-- Test product expressions
productThreeExpr :: TestExpr
productThreeExpr = Lam (ProductT [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat*Bool*Nat ->
    CaseAnalysis $ Case (Binding VZ)                                       -- case x of
      $ CaseBranches                                                       --
        ((CaseBranch (MatchProduct [zPat,Bind,zPat]) (Binding VZ))         -- Z,y,Z -> y
         :| [CaseBranch (MatchProduct [Bind,Bind,zPat]) (Binding VZ)]      -- x,y,Z -> y
        )                                                                  --
        (Just                                                              --
            falseTerm                                                      -- _ -> False
        )

productThreeExprType :: TestType
productThreeExprType = Arrow (ProductT [natTypeName,boolTypeName,natTypeName]) boolTypeName
productThreeText :: Text
productThreeText = Text.unlines
  ["\\(* Nat Bool Nat) (CASE 0"
  ,"                    (| (* (+0 (*)) (?) (+0 (*))) (0))"
  ,"                    (| (* (?)      (?) (+0 (*))) (0))"
  ,""
  ,"                    (+0 (*) (*) (*))"
  ,"                  )"
  ]

