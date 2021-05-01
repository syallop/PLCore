{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Text.Expr.Binding
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr that test interactions with bindings.
-}
module PL.Test.Expr.Binding
  ( TestBindingSources (..)
  , bindingTestCases
  )
  where

import PL.Expr
import PL.Reduce
import PL.Type
import PL.TypeCheck
import PL.Var

import Data.Text (Text)

import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestBindingSources = TestBindingSources
  { _bindingTestCase       :: Source
  , _buriedBindingTestCase :: Source
  , _doubleBuriedBindingTestCase :: Source
  , _buriedBindingsDontMoveTestCase :: Source
  }

bindingTestCases
  :: TestBindingSources
  -> [(Text, ExprTestCase)]
bindingTestCases t =
  [("simple binding", bindingTestCase       . _bindingTestCase       $ t)
  ,("buried binding", buriedBindingTestCase . _buriedBindingTestCase $ t)
  ,("double burried binding", doubleBuriedBindingTestCase . _doubleBuriedBindingTestCase $ t)
  ,("buried bindings don't move", buriedBindingsDontMoveTestCase . _buriedBindingsDontMoveTestCase $ t)
  ]

bindingTestCase
  :: Source
  -> ExprTestCase
bindingTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam unitTypeName $ Binding VZ

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam unitTypeName $ Binding VZ

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = Arrow unitTypeName unitType

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam unitTypeName $ Binding VZ
      , _reducesToWhenApplied =
          [ ("Unbound"
            , []
            , Just $ Lam (Named "Unit") $ Binding VZ
            )

          , ("Bound"
            , [(`App` unitTerm)]
            , Just unitTerm
            )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = unitTypeCtx

-- Test that burried bindings are incremented
--
-- \t. ( (\(t->t). (\t. 1)) (\t. 1)
--  |      |____________|        |
--  -----------------------------
--
-- Should reduce:
--
-- \t. \t. \t. 2
buriedBindingTestCase
  :: Source
  -> ExprTestCase
buriedBindingTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Binding $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Binding $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName boolTypeName))

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Binding $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))
      , _reducesToWhenApplied =
          [
           ( "Bindings are adjusted correctly when buried under lambdas"
           , []
           , Just $ Lam boolTypeName $ Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS $ VZ -- Note the original binding should have been increased from 1 to 2 as it's application moved its binding further away.
           )

          ,( "Buried bindings reduce to the correct value"
           , [(`App` trueTerm)
             ,(`App` falseTerm)
             ]
           , Just $ Lam boolTypeName $ trueTerm
           )
          ]

      , _underEvaluationCtx = undefined
      , _evaluatesTo = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx

-- Test that burried bindings are incremented by more than 1 where necessary
--
-- \t. ( (\(t->t). (\t. (\t. 2))) (\t. 1)
-- |       |_________________|         |
-- ------------------------------------
--
-- Should reduce
--
-- \t. \t. \t. t. 3
doubleBuriedBindingTestCase
  :: Source
  -> ExprTestCase
doubleBuriedBindingTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

      , _underResolveCtx = undefined
      , _resolvesTo = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName boolTypeName)))

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))
      , _reducesToWhenApplied =
          [
           ( "Bindings are adjusted correctly when buried under lambdas"
           , []
           , Just $ Lam boolTypeName $ Lam boolTypeName $ Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS $ VS $ VZ
           )

          ,( "Buried bindings reduce to the correct value"
           , [(`App` trueTerm)
             ,(`App` falseTerm)
             ,(`App` falseTerm)
             ]
           , Just $ Lam boolTypeName $ trueTerm
           )
          ]

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx

-- Test that burried bindings are not incremented when the binding does not move
--
-- Should reduce:
--
-- \t. \t. \t. 2

-- \t ( (\t. 0) (0))
-- |      |__|   |
-- --------------
--
-- Should reduce:
--
-- \t 0.
--
-- NOT
--
-- \t 1.
buriedBindingsDontMoveTestCase
  :: Source
  -> ExprTestCase
buriedBindingsDontMoveTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = Lam boolTypeName $ App (Lam boolTypeName (Binding VZ))
                                 (Binding VZ)

      , _underResolveCtx = undefined
      , _resolvesTo      = Lam boolTypeName $ App (Lam boolTypeName (Binding VZ))
                                 (Binding VZ)

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = Arrow boolTypeName boolTypeName

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = Lam boolTypeName $ App (Lam boolTypeName (Binding VZ))
                                 (Binding VZ)
      ,_reducesToWhenApplied =
          [
           ( "Bindings are not adjusted when they don't move"
           , []
           , Just $ Lam boolTypeName $ Binding $ VZ
           )
          ]

      , _underEvaluationCtx     = undefined
      , _evaluatesTo            = undefined
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = boolTypeCtx

