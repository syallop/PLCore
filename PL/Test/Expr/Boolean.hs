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
import PL.TypeCheck
import PL.Var

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

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
      }
  where
    ctx = boolTypeCtx

