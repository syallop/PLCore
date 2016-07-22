{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Sum
  ( sumThreeExpr
  , sumThreeExprType
  , sumThreeText
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

-- Test case analysis on a sum type with overlapping members
sumThreeExpr :: TestExpr
sumThreeExpr = Lam (SumT [natTypeName,boolTypeName,natTypeName]) $   -- \x : Nat|Bool|Nat ->
    CaseAnalysis $ Case (Binding VZ)                                 -- case x of
      $ CaseBranches                                                 --
        ((CaseBranch (MatchSum 0 $ sPat Bind) (Binding VZ))          --  0| S n   -> n
         :| [CaseBranch (MatchSum 0   zPat)      zTerm               --  0| Z     -> Z
            ,CaseBranch (MatchSum 1   falsePat)  zTerm               --  1| False -> Z
            ,CaseBranch (MatchSum 1   truePat)   (sTerm `App` zTerm) --  1| True  -> S Z
            ,CaseBranch (MatchSum 2 $ sPat Bind) zTerm               --  2| S n   -> Z
            ,CaseBranch (MatchSum 2   zPat)      (sTerm `App` zTerm) --  2| Z     -> S Z
            ]
        )
        Nothing
sumThreeExprType :: TestType
sumThreeExprType = Arrow (SumT [natTypeName,boolTypeName,natTypeName]) natTypeName
sumThreeText :: Text
sumThreeText = Text.unlines
  ["\\(+Nat Bool Nat) (CASE 0"
  ,"                   (| (+0 +1 ?)    (0))"
  ,"                   (| (+0 +0 (*))  (+0 (*) (*) Nat))"
  ,"                   (| (+1 +0 (*))  (+0 (*) (*) Nat))"
  ,"                   (| (+1 +1 (*))  (@ (\\Nat (+1 0 (*) Nat))  (+0 (*) (*) Nat) ))"
  ,"                   (| (+2 +1 ?)    (+0 (*) (*) Nat))"
  ,"                   (| (+2 +0 (*))  (@ (\\Nat (+1 0 (*) Nat))  (+0 (*) (*) Nat) ))"
  ,"                 )"
  ]

