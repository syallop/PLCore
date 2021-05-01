{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-|
Module      : PL.Test.Expr.Natural
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Natural' type.
-}
module PL.Test.Expr.Natural
  ( natTypeCtx
  , natTypeName
  , natType
  , zTerm
  , sTerm
  , zPat
  , sPat

  , zero
  , succ
  , one
  , two
  , three
  , four

  , subTwoExprTestCase

  , TestNaturalSources (..)
  , naturalTestCases
  )
  where

import PL.Case
import PL.Expr
import PL.Reduce
import PL.Type
import PL.Pattern
import PL.TypeCheck
import PL.Var

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestNaturalSources = TestNaturalSources
  { _subTwoTestCase :: Source
  }

naturalTestCases
  :: TestNaturalSources
  -> [(Text, ExprTestCase)]
naturalTestCases t =
  [ ("subtract two", subTwoExprTestCase . _subTwoTestCase $ t)
  ]

-- Test nested pattern matching
-- n > 2     ~> n-2
-- otherwise ~> 0
subTwoExprTestCase
  :: Source
  -> ExprTestCase
subTwoExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   =
          Lam natTypeName $                                       -- \n : Nat ->
            CaseAnalysis $ Case (Binding $ VZ)                    -- case n of
              $ CaseBranches                                      --
                (CaseBranch (sPat $ sPat Bind) (Binding VZ) :| [] --   S S n -> n
                )                                                 --
                (Just                                             --
                      zTerm                                       --   _     -> Z
                )

      , _underResolveCtx = undefined
      , _resolvesTo      =
          Lam natTypeName $                                       -- \n : Nat ->
            CaseAnalysis $ Case (Binding $ VZ)                    -- case n of
              $ CaseBranches                                      --
                (CaseBranch (sPat $ sPat Bind) (Binding VZ) :| [] --   S S n -> n
                )                                                 --
                (Just                                             --
                      zTerm                                       --   _     -> Z
                )

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = Arrow natType natType

      , _underReductionCtx    = topReductionCtx ctx
      , _reducesTo            =
          Lam natTypeName $                                       -- \n : Nat ->
            CaseAnalysis $ Case (Binding $ VZ)                    -- case n of
              $ CaseBranches                                      --
                (CaseBranch (sPat $ sPat Bind) (Binding VZ) :| [] --   S S n -> n
                )                                                 --
                (Just                                             --
                      zTerm                                       --   _     -> Z
                )

      , _reducesToWhenApplied =
          [ ( "3 - 2 = 1"
            , [(`App` three)]
            , Just one
            )

          , ( "2 -2 = 0"
            , [(`App` two)]
            , Just zero
            )

          , ( "1 - 2 = 0"
            , [(`App` one)]
            , Just zero
            )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo        = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = natTypeCtx

