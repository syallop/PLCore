{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : PL.Test.Expr.List
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Expr using a heterogenous 'List' type parameterised by some
element type.
-}
module PL.Test.Expr.List
  ( listTypeCtx
  , listTypeName
  , listType
  , listSumType
  , emptyTerm
  , consTerm

  , emptyListTestCase

  , TestListSources (..)
  , listTestCases
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

import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import PL.Test.Expr.Natural
import PL.Test.ExprTestCase
import PL.Test.Source
import PL.Test.Shared

data TestListSources = TestListSources
  { _emptyListTestCase     :: Source
  }

listTestCases
  :: TestListSources
  -> [(Text, ExprTestCase)]
listTestCases t =
  [ ("empty list"    , emptyListTestCase . _emptyListTestCase $ t)
  ]

-- Empty lists can be instantiated with types.
emptyListTestCase
  :: Source
  -> ExprTestCase
emptyListTestCase src
  = ExprTestCase
      { _underTypeCheckCtx = topTypeCheckCtx ctx
      , _isExpr       = e
      , _typed        = ty
      , _parsesFrom   = src

      , _reducesTo = stripComments e
      , _reducesToWhenApplied = reduces
      }
  where
    ctx = sharedTypeCtx

    -- [] :: forall a. [a]
    e   = emptyTerm

    ty :: Type
    ty  = BigArrow Kind $ (SumT $ NE.fromList [EmptyProductT, ProductT [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]])

    reduces = [("[] : [Nat]"
               , [(`BigApp` natTypeName)
                 ]
               , Just $ Sum EmptyProduct 0 $ NE.fromList $ [EmptyProductT,ProductT [natTypeName, TypeApp listTypeName natTypeName]]
               )

              ,("[] : [[Bool]]"
              , [(`BigApp` (TypeApp listTypeName boolTypeName))]
              , Just $ Sum EmptyProduct 0 $ NE.fromList $ [EmptyProductT,ProductT [TypeApp listTypeName boolTypeName, TypeApp listTypeName (TypeApp listTypeName boolTypeName)]]
              )
              ]

