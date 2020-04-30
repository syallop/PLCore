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
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Pattern
import PL.Var

import Data.Text (Text)
import Data.Maybe
import Data.Monoid hiding (Sum)
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import PL.Test.Expr.Natural
import PL.Test.Expr.Boolean

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

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

    ,_reducesTo = stripComments e
    ,_reducesToWhenApplied = reduces
    }
  where
    ctx = natTypeCtx <> boolTypeCtx
    e   = Lam (SumT $ NE.fromList [natTypeName,boolTypeName,natTypeName]) $ -- \x : Nat|Bool|Nat ->
            CaseAnalysis $ Case (Binding VZ)                                          -- case x of
              $ CaseBranches                                                          --
                (CaseBranch (SumPattern 0 $ sPat Bind) (Binding VZ)                     --  0| S n   -> n
                 :| [CaseBranch (SumPattern 0   zPat)      zTerm                        --  0| Z     -> Z
                    ,CaseBranch (SumPattern 1   falsePat)  zTerm                        --  1| False -> Z
                    ,CaseBranch (SumPattern 1   truePat)   (App sTerm zTerm)            --  1| True  -> S Z
                    ,CaseBranch (SumPattern 2 $ sPat Bind) zTerm                        --  2| S n   -> Z
                    ,CaseBranch (SumPattern 2   zPat)      (App sTerm zTerm)            --  2| Z     -> S Z
                    ]
                )
                Nothing
    ty  = Arrow (SumT $ NE.fromList [natTypeName,boolTypeName,natTypeName]) natTypeName

    reduces =
      [ ("+1 False"
        ,[(`App` (Sum falseTerm 1 $ NE.fromList $ [natTypeName,boolTypeName,natTypeName]))]
        ,Just zero
        )

      , ("+0 0"
        ,[(`App` (Sum zero 0 $ NE.fromList $ [natTypeName,boolTypeName,natTypeName]))]
        ,Just zero
        )

      , ("+2 0"
        ,[(`App` (Sum zero 2 $ NE.fromList $[natTypeName,boolTypeName,natTypeName]))]
        ,Just one
        )
      ]

