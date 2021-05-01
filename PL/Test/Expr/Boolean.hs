{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Text.Expr.Boolean
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a 'Boolean' type.
-}
module PL.Test.Expr.Boolean
  ( TestBooleanSources (..)
  , booleanTestCases
  , andExprTestCase
  )
  where

import PL.Case
import PL.Expr
import PL.Reduce
import PL.Type
import PL.TypeCheck
import PL.Var

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestBooleanSources = TestBooleanSources
  { _andTestCase :: Source
  }

booleanTestCases
  :: TestBooleanSources
  -> [(Text, ExprTestCase)]
booleanTestCases t =
  [("and", andExprTestCase . _andTestCase $ t)
  ]

-- Boolean and
andExprTestCase
  :: Source
  -> ExprTestCase
andExprTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam boolTypeName $ Lam boolTypeName $                   -- \x:Bool y:Bool ->
        CaseAnalysis $ Case (Binding VZ)                                      -- case y of
          $ CaseBranches                                                      --
            (CaseBranch falsePat falseTerm :| []                              --     False -> False
            )                                                                 --
            (Just                                                             --     _      ->
                (CaseAnalysis $ Case (Binding $ VS VZ)                        --               case x of
                  $ CaseBranches                                              --
                    (CaseBranch falsePat falseTerm :|[]                       --                 False -> False
                    )                                                         --
                    (Just                                                     --                        _     ->
                        trueTerm                                              --                                 True
                    )
                )
            )

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam boolTypeName $ Lam boolTypeName $                   -- \x:Bool y:Bool ->
              CaseAnalysis $ Case (Binding VZ)                                     -- case y of
                $ CaseBranches                                                     --
                  (CaseBranch falsePat falseTerm :| []                             --     False -> False
                  )                                                                --
                  (Just                                                            --     _      ->
                      (CaseAnalysis $ Case (Binding $ VS VZ)                       --               case x of
                        $ CaseBranches                                             --
                          (CaseBranch falsePat falseTerm :|[]                      --                 False -> False
                          )                                                        --
                          (Just                                                    --                        _     ->
                              trueTerm                                             --                                 True
                          )
                      )
                  )

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed             = Arrow boolType (Arrow boolType boolType)

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam boolTypeName $ Lam boolTypeName $                         -- \x:Bool y:Bool ->
              CaseAnalysis $ Case (Binding VZ)                                     -- case y of
                $ CaseBranches                                                     --
                  (CaseBranch falsePat falseTerm :| []                             --     False -> False
                  )                                                                --
                  (Just                                                            --     _      ->
                      (CaseAnalysis $ Case (Binding $ VS VZ)                       --               case x of
                        $ CaseBranches                                             --
                          (CaseBranch falsePat falseTerm :|[]                      --                 False -> False
                          )                                                        --
                          (Just                                                    --                        _     ->
                              trueTerm                                             --                                 True
                          )
                      )
                  )
      ,_reducesToWhenApplied =
          [ ("true"
            , [(`App` trueTerm)]
            , Just $ Lam (Named "Bool") $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
                (CaseBranch falsePat falseTerm :| [])
                (Just trueTerm)
            )

          , ("false true"
            , [(`App` falseTerm), (`App` trueTerm)]
            , Just falseTerm
            )

          , ("true true"
            , [(`App` trueTerm), (`App` trueTerm)]
            , Just trueTerm
            )
          ]

      ,_underEvaluationCtx     = undefined
      ,_evaluatesTo            = undefined
      ,_evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx

