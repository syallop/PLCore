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

data TestBindingSources = TestBindingSources
  { _bindingTestCase       :: Source
  , _buriedBindingTestCase :: Source
  , _doubleBuriedBindingTestCase :: Source
  }

bindingTestCases
  :: TestBindingSources
  -> [(Text, ExprTestCase)]
bindingTestCases t =
  [("simple binding", bindingTestCase       . _bindingTestCase       $ t)
  ,("buried binding", buriedBindingTestCase . _buriedBindingTestCase $ t)
  ,("double burried binding", doubleBuriedBindingTestCase . _doubleBuriedBindingTestCase $ t)
  ]

bindingTestCase
  :: Source
  -> ExprTestCase
bindingTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = unitTypeCtx
    e   = Lam unitTypeName $ Binding VZ
    ty  = Arrow unitTypeName unitType

    reductions =
      [ ("Unbound"
        , []
        , Just $ Lam (Named "Unit") $ Binding VZ
        )

      , ("Bound"
        , [(`App` unitTerm)]
        , Just unitTerm
        )
      ]

buriedBindingTestCase
  :: Source
  -> ExprTestCase
buriedBindingTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = boolTypeCtx

    -- \t. ( (\(t->t). (\t. 1)) (\t. 1)
    --  |      |____________|        |
    --  -----------------------------
    --
    -- Should reduce:
    --
    -- \t. \t. \t. 2
    e   = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Binding $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

    ty  = Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName boolTypeName))

    reductions =
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

doubleBuriedBindingTestCase
  :: Source
  -> ExprTestCase
doubleBuriedBindingTestCase src
  = ExprTestCase
      { _underTypeCtx = ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      ,_reducesTo = stripComments e
      ,_reducesToWhenApplied = reductions
      }
  where
    ctx = boolTypeCtx


    -- \t. ( (\(t->t). (\t. (\t. 2))) (\t. 1)
    -- |       |_________________|         |
    -- ------------------------------------
    --
    -- Should reduce
    --
    -- \t. \t. \t. t. 3
    e   = Lam boolTypeName $ App (Lam (Arrow boolTypeName boolTypeName) (Lam boolTypeName $ Lam boolTypeName $ Binding $ VS $ VS VZ))
                                 (Lam boolTypeName (Binding $ VS VZ))

    ty  = Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName (Arrow boolTypeName boolTypeName)))

    reductions =
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

