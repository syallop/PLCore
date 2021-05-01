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
  , emptyTerm
  , consTerm

  , emptyListTestCase

  , TestListSources (..)
  , listTestCases
  )
  where

import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.TypeCheck
import PL.Var

import Data.Text (Text)
import qualified Data.List.NonEmpty as NE

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
  [ ("empty list" , emptyListTestCase . _emptyListTestCase $ t)
  ]

-- Empty lists can be instantiated with types.
-- [] :: forall a. [a]
emptyListTestCase
  :: Source
  -> ExprTestCase
emptyListTestCase src
  = ExprTestCase
      { _parsesFrom = src
      , _parsesTo   = emptyTerm

      , _underResolveCtx = undefined
      , _resolvesTo      = emptyTerm

      , _underTypeCheckCtx = topTypeCheckCtx ctx
      , _typed = BigArrow Kind $ (SumT $ NE.fromList [ EmptyProductT
                                                     , ProductT [ TypeBinding $ TyVar VZ
                                                                , TypeApp listTypeName (TypeBinding $ TyVar VZ)
                                                                ]
                                                     ]
                                 )

      , _underReductionCtx = topReductionCtx ctx
      , _reducesTo = emptyTerm
      , _reducesToWhenApplied = []
         {-
           [("[] : [Nat]"
             , [(`BigApp` natTypeName)
               ]
             , Just $ Sum EmptyProduct 0 $ NE.fromList $ [EmptyProductT, ProductT [natTypeName, TypeApp listTypeName natTypeName]]
             )

            ,("[] : [[Bool]]"
            , [(`BigApp` (TypeApp listTypeName boolTypeName))]
            , Just $ Sum EmptyProduct 0 $ NE.fromList $ [EmptyProductT, ProductT [TypeApp listTypeName boolTypeName, TypeApp listTypeName (TypeApp listTypeName boolTypeName)]]
            )
            ]
        -}

      , _underEvaluationCtx = undefined
      , _evaluatesTo = emptyTerm
      , _evaluatesToWhenApplied = undefined
      }
  where
    ctx = sharedTypeCtx

