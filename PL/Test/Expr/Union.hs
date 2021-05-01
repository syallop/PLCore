{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Test.Expr.Union
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using the 'Union' type.
-}
module PL.Test.Expr.Union
  ( unionTwoExprTestCase

  , TestUnionSources (..)
  , unionTestCases
  )
  where

import PL.Case
import PL.Expr
import PL.Reduce
import PL.Type
import PL.TypeCheck
import PL.Var
import PL.Pattern

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import Data.Text (Text)

import PL.Test.Expr.Natural

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestUnionSources = TestUnionSources
  { _unionTwoTestCase :: Source
  }

unionTestCases
  :: TestUnionSources
  -> [(Text, ExprTestCase)]
unionTestCases t =
  [ ("union of two types", unionTwoExprTestCase . _unionTwoTestCase $ t)
  ]

-- : <Nat|Bool> -> Bool
unionTwoExprTestCase
  :: Source
  -> ExprTestCase
unionTwoExprTestCase src =
  ExprTestCase
    { _parsesFrom = src
    , _parsesTo   = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $    -- \x : <Nat|Bool>
            CaseAnalysis $ Case (Binding VZ)                                    -- case x of
              $ CaseBranches                                                    --
                (CaseBranch (UnionPattern natTypeName   zPat)      falseTerm      -- Nat | Z    -> False
                 :| [CaseBranch (UnionPattern natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
                    ,CaseBranch (UnionPattern boolTypeName  truePat)   trueTerm   -- Bool| True -> True
                    ]                                                           --
                )                                                               --
                (Just                                                           --
                    falseTerm                                                   -- _          -> False
                )

    , _underResolveCtx = undefined
    , _resolvesTo      = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $    -- \x : <Nat|Bool>
            CaseAnalysis $ Case (Binding VZ)                                    -- case x of
              $ CaseBranches                                                    --
                (CaseBranch (UnionPattern natTypeName   zPat)      falseTerm      -- Nat | Z    -> False
                 :| [CaseBranch (UnionPattern natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
                    ,CaseBranch (UnionPattern boolTypeName  truePat)   trueTerm   -- Bool| True -> True
                    ]                                                           --
                )                                                               --
                (Just                                                           --
                    falseTerm                                                   -- _          -> False
                )

    , _underTypeCheckCtx = topTypeCheckCtx ctx
    , _typed = Arrow (UnionT $ Set.fromList [natTypeName,boolTypeName]) boolTypeName

    , _underReductionCtx = topReductionCtx ctx
    , _reducesTo = Lam (UnionT $ Set.fromList [natTypeName,boolTypeName]) $    -- \x : <Nat|Bool>
            CaseAnalysis $ Case (Binding VZ)                                    -- case x of
              $ CaseBranches                                                    --
                (CaseBranch (UnionPattern natTypeName   zPat)      falseTerm      -- Nat | Z    -> False
                 :| [CaseBranch (UnionPattern natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
                    ,CaseBranch (UnionPattern boolTypeName  truePat)   trueTerm   -- Bool| True -> True
                    ]                                                           --
                )                                                               --
                (Just                                                           --
                    falseTerm                                                   -- _          -> False
                )
    , _reducesToWhenApplied =
          [("∪ 1 Nat Nat Bool"
           ,[(`App` (Union one natTypeName $ Set.fromList [natTypeName,boolTypeName]))]
           ,Just trueTerm
           )

          ,("∪ False Bool Nat Bool"
           ,[(`App` (Union falseTerm boolTypeName $ Set.fromList [natTypeName,boolTypeName]))]
           ,Just falseTerm
           )

          ,("∪ True Bool Bool Nat"
           ,[(`App` (Union falseTerm boolTypeName $ Set.fromList [boolTypeName,natTypeName]))]
           ,Just falseTerm
           )
          ]

    , _underEvaluationCtx     = undefined
    , _evaluatesTo            = undefined
    , _evaluatesToWhenApplied = undefined
    }
  where
    ctx = boolTypeCtx <> natTypeCtx

