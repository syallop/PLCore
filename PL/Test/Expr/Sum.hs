{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Sum
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'sum' type.
-}
module PL.Test.Expr.Sum
  ( sumThreeExprTestCase

  , TestSumSources (..)
  , sumTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Text (Text)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))

import PL.Test.Expr.Natural
import PL.Test.Expr.Boolean

import PL.Test.ExprTestCase
import PL.Test.Source

data TestSumSources = TestSumSources
  { _sumThreeTestCase :: Source
  }

sumTestCases
  :: TestSumSources
  -> [(Text, ExprTestCase)]
sumTestCases t =
  [ ("sum three types", sumThreeExprTestCase . _sumThreeTestCase $ t)
  ]

-- Test case analysis on a sum type with overlapping members
sumThreeExprTestCase
  :: Source
  -> ExprTestCase
sumThreeExprTestCase src =
  ExprTestCase
    { _underTypeCtx = ctx
    , _isExpr       = e
    , _typed        = ty
    , _parsesFrom   = src
    }
  where
    ctx = fromJust $ natTypeCtx <> boolTypeCtx
    e   = fixExpr $ Lam (fixType $ SumT [natTypeName,boolTypeName,natTypeName]) $ fixExpr $    -- \x : Nat|Bool|Nat ->
            CaseAnalysis $ Case (fixExpr $ Binding VZ)                               -- case x of
              $ CaseBranches                                                         --
                (CaseBranch (MatchSum 0 $ sPat Bind) (fixExpr $ Binding VZ)          --  0| S n   -> n
                 :| [CaseBranch (MatchSum 0   zPat)      zTerm                       --  0| Z     -> Z
                    ,CaseBranch (MatchSum 1   falsePat)  zTerm                       --  1| False -> Z
                    ,CaseBranch (MatchSum 1   truePat)   (fixExpr $ App sTerm zTerm) --  1| True  -> S Z
                    ,CaseBranch (MatchSum 2 $ sPat Bind) zTerm                       --  2| S n   -> Z
                    ,CaseBranch (MatchSum 2   zPat)      (fixExpr $ App sTerm zTerm) --  2| Z     -> S Z
                    ]
                )
                Nothing
    ty  = fixType $ Arrow (fixType $ SumT [natTypeName,boolTypeName,natTypeName]) natTypeName

