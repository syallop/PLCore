{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Text.Expr.SelfTypes
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr that test interactions with self types.
-}
module PL.Test.Expr.SelfTypes
  ( TestSelfTypeSources (..)
  , selfTypeTestCases
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Pattern
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

data TestSelfTypeSources = TestSelfTypeSources
  { _selfTypesCanBeMentionedTestCase :: Source
  , _selfTypesCanBeReturnedTestCase  :: Source

  , _selfTypesCanBeConstructedTestCase       :: Source
  , _nestedSelfTypesCanBeConstructedTestCase :: Source

  , _selfTypesCanBeDeconstructedTestCase :: Source

  , _successorTestCase :: Source
  }

selfTypeTestCases
  :: TestSelfTypeSources
  -> [(Text, ExprTestCase)]
selfTypeTestCases t =
  [("Self types can be mentioned", selfTypesCanBeMentionedTestCase . _selfTypesCanBeMentionedTestCase $ t)
  ,("Self types can be returned", selfTypesCanBeReturnedTestCase   . _selfTypesCanBeReturnedTestCase  $ t)
  ,("Self types can be constructed", selfTypesCanBeConstructedTestCase . _selfTypesCanBeConstructedTestCase $ t)
  ,("Nested self types can be constructed", nestedSelfTypesCanBeConstructedTestCase . _nestedSelfTypesCanBeConstructedTestCase $ t)
  ,("Self types can be deconstructed", selfTypesCanBeDeconstructedTestCase . _selfTypesCanBeDeconstructedTestCase $ t)
  ,("Successor function", successorTestCase . _successorTestCase $ t)
  ]

-- Test that self types can be mentioned at all by including them as an unused
-- branch in a sum expression.
--
-- "Sum of empty product and anonymous Nat"
-- +0 (*) (*) (μKIND (+ (*) %))
selfTypesCanBeMentionedTestCase
  :: Source
  -> ExprTestCase
selfTypesCanBeMentionedTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Sum EmptyProduct 0 $ NE.fromList [ EmptyProductT
                                                       , TypeMu Kind $ SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]
                                                       ]

      , _underResolveCtx = undefined
      , _resolvesTo      = Sum EmptyProduct 0 $ NE.fromList [ EmptyProductT
                                                            , TypeMu Kind $ SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]
                                                            ]

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = SumT $ NE.fromList [ EmptyProductT
                                    , TypeMu Kind $ SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]
                                    ]

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Sum EmptyProduct 0 $ NE.fromList [ EmptyProductT
                                                      , TypeMu Kind $ SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]
                                                      ]
      , _reducesToWhenApplied =
          [
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Test that self types can be calculated as the result type of a lambda (by
-- returning one via a binding)
--
-- "Identity function on Nat"
-- \(μKIND (+ (*) %)) 0
selfTypesCanBeReturnedTestCase
  :: Source
  -> ExprTestCase
selfTypesCanBeReturnedTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ)

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ)


      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = let t = (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) in Arrow t t

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ)
      , _reducesToWhenApplied =
          [
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Test that expressions can be constructed that are accepted as a self type (by
-- supplying one to a lambda).
--
-- "Apply Nat to identity function
-- "
-- @ (\(μKIND (+ (*) %)) 0) (+0 (*) (*) %)
selfTypesCanBeConstructedTestCase
  :: Source
  -> ExprTestCase
selfTypesCanBeConstructedTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = App (Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ))
                          (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underResolveCtx = undefined
      , _resolvesTo      = App (Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ))
                               (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]
      , _reducesToWhenApplied =
          [
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Test that expressions can be constructed that are accepted as a self type
-- when the expression nests the self-type references (by
-- supplying one to a lambda).
--
--"Apply Nat one to identity function
-- "
-- @ (\(μKIND (+ (*) %)) 0) (+1 (+0 (*) (*) %) (*) %)
nestedSelfTypesCanBeConstructedTestCase
  :: Source
  -> ExprTestCase
nestedSelfTypesCanBeConstructedTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = App (Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ))
                          (Sum (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underResolveCtx = undefined
      , _resolvesTo      = App (Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Binding VZ))
                               (Sum (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Sum (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding]
      , _reducesToWhenApplied =
          [
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Test that expressions with self-types can be deconstructed.
--
-- \(μKIND (+ (*) %))
--  (CASE 0
--    (| (+0 ?) (+0 * * *))
--    (| (+1 ?) (+1 * * *))
--  )
selfTypesCanBeDeconstructedTestCase
  :: Source
  -> ExprTestCase
selfTypesCanBeDeconstructedTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
                        (NE.fromList $
                         [ CaseBranch (SumPattern 0 Bind) (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT,EmptyProductT])
                         , CaseBranch (SumPattern 1 Bind) (Sum EmptyProduct 1 $ NE.fromList [EmptyProductT,EmptyProductT])
                         ]
                        )
                        Nothing

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
                            (NE.fromList $
                             [ CaseBranch (SumPattern 0 Bind) (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT,EmptyProductT])
                             , CaseBranch (SumPattern 1 Bind) (Sum EmptyProduct 1 $ NE.fromList [EmptyProductT,EmptyProductT])
                             ]
                            )
                            Nothing

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = Arrow (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]))
                       (SumT $ NE.fromList [EmptyProductT,EmptyProductT])

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) $ CaseAnalysis $ Case (Binding VZ) $ CaseBranches
                          (NE.fromList $
                           [ CaseBranch (SumPattern 0 Bind) (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT,EmptyProductT])
                           , CaseBranch (SumPattern 1 Bind) (Sum EmptyProduct 1 $ NE.fromList [EmptyProductT,EmptyProductT])
                           ]
                          )
                          Nothing

      , _reducesToWhenApplied =
          [ ("Zero"
            , [ (`App` (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding])) -- (+0 (*) (*) %)
              ]
            , Just $ Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, EmptyProductT]
            )

          , ("One"
            , [ (`App` (Sum (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])) -- (+1 (+0 (*) (*) %) (*) %)
              ]
            , Just $ Sum EmptyProduct 1 $ NE.fromList [EmptyProductT, EmptyProductT]
            )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Tests that the outer lambda is peeking inside its type and binding the
-- self-type for use in the RHS. Otherwise the RHS would have to expand the % a
-- level.
--
-- \(μKIND (+ (*) %)) (+1 (0) (*) %)
successorTestCase
  :: Source
  -> ExprTestCase
successorTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Sum (Binding VZ) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Sum (Binding VZ) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = let t = TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding]) in Arrow t t

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam (TypeMu Kind (SumT $ NE.fromList [EmptyProductT, TypeSelfBinding])) (Sum (Binding VZ) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding])
      , _reducesToWhenApplied =
          [("zero"
           , [(`App` (Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding]))]
           , Just $ Sum ((Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, TypeSelfBinding])) 1 $ NE.fromList [EmptyProductT, TypeSelfBinding]
           )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

