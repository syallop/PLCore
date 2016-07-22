{-# LANGUAGE OverloadedStrings #-}
module ExprSpec.Union
  ( unionTwoExpr
  , unionTwoExprType
  , unionTwoText
  )
  where

import ExprSpec.Boolean

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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import qualified Data.Set as Set

import ExprSpec.Boolean
import ExprSpec.Natural

type TestType = Type TyVar
type TestExpr = Expr Var TestType TyVar

-- : <Nat|Bool> -> Bool
unionTwoExpr :: TestExpr
unionTwoExpr = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $ -- \x : <Nat|Bool>
    CaseAnalysis $ Case (Binding VZ)                                    -- case x of
      $ CaseBranches                                                    --
        ((CaseBranch (MatchUnion natTypeName   zPat)      falseTerm)    -- Nat | Z    -> False
         :| [CaseBranch (MatchUnion natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
            ,CaseBranch (MatchUnion boolTypeName  truePat)   trueTerm   -- Bool| True -> True
            ]                                                           --
        )                                                               --
        (Just                                                           --
            falseTerm                                                   -- _          -> False
        )
unionTwoExprType :: TestType
unionTwoExprType = Arrow (UnionT $ Set.fromList [natTypeName,boolTypeName]) boolTypeName
unionTwoText :: Text
unionTwoText = Text.unlines
  ["\\(∪ Bool Nat) (CASE 0"
  ,"                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))"
  ,"                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))"
  ,"                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))"
  ,""
  ,"                (+0 (*) (*) (*))"
  ,"              )"
  ]

