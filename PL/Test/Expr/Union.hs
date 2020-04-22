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

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.FixExpr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.FixType
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import PLParser

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as Text
import qualified Data.Set as Set

import PL.Test.Expr.Boolean
import PL.Test.Expr.Natural

import PL.Test.ExprTestCase
import PL.Test.Source

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
    { _underTypeCtx = ctx
    , _isExpr       = e
    , _typed        = ty
    , _parsesFrom   = src
    }
  where
    ctx = fromJust $ boolTypeCtx <> natTypeCtx
    e   = Lam (fixType $ UnionT $ Set.fromList [natTypeName,boolTypeName]) $    -- \x : <Nat|Bool>
            CaseAnalysis $ Case (Binding VZ)                                    -- case x of
              $ CaseBranches                                                    --
                (CaseBranch (MatchUnion natTypeName   zPat)      falseTerm      -- Nat | Z    -> False
                 :| [CaseBranch (MatchUnion natTypeName $ sPat Bind) trueTerm   -- Nat | S n  -> True
                    ,CaseBranch (MatchUnion boolTypeName  truePat)   trueTerm   -- Bool| True -> True
                    ]                                                           --
                )                                                               --
                (Just                                                           --
                    falseTerm                                                   -- _          -> False
                )
    ty  = fixType $ Arrow (fixType $ UnionT $ Set.fromList [natTypeName,boolTypeName]) boolTypeName

