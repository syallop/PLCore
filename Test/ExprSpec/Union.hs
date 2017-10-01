{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ExprSpec.Union
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'Union' type.
-}
module ExprSpec.Union
  ( unionTwoExprTestCase
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

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import qualified Data.Set as Set

import ExprSpec.Boolean
import ExprSpec.Natural

import ExprTestCase

-- : <Nat|Bool> -> Bool
unionTwoExprTestCase :: ExprTestCase
unionTwoExprTestCase = ExprTestCase
  {_underTypeCtx = ctx
  ,_isExpr       = e
  ,_typed        = ty
  ,_parsesFrom   = txt
  }
  where
    ctx = fromJust $ boolTypeCtx <> natTypeCtx
    e   = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $ -- \x : <Nat|Bool>
            CaseAnalysis $ Case (Binding VZ)                                    -- case x of
              $ CaseBranches                                                    --
                (CaseBranch (MatchUnion natTypeName   zPat)      falseTerm    -- Nat | Z    -> False
                 :| [CaseBranch (MatchUnion natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
                    ,CaseBranch (MatchUnion boolTypeName  truePat)   trueTerm   -- Bool| True -> True
                    ]                                                           --
                )                                                               --
                (Just                                                           --
                    falseTerm                                                   -- _          -> False
                )
    ty  = Arrow (UnionT $ Set.fromList [natTypeName,boolTypeName]) boolTypeName
    txt = Text.unlines
      ["λ(∪ Bool Nat) (CASE 0"
      ,"                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))"
      ,"                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))"
      ,"                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))"
      ,""
      ,"                (+0 (*) (*) (*))"
      ,"              )"
      ]

