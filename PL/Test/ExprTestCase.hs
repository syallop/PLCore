{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  , RankNTypes
  #-}
{-|
Module      : PL.Test.ExprTestCase
Copyright   : (c) Samuel A. Yallop, 2017
Maintainer  : syallop@gmail.com
Stability   : experimental

Functions for testing expression parsing, typechecking, reduction, etc.
Also exports 'ExprTestCase' which encapsulates an example which can have all of these properties tested.
-}
module PL.Test.ExprTestCase
  ( ExprTestCase(..)
  , ReductionTestCase
  )
  where

import PL.Expr
import PL.Commented
import PL.Reduce
import PL.Type
import PL.Resolve
import PL.TypeCheck
import PL.Evaluate

import Data.Text (Text)

-- ExprTestCase collects together common parameters for testcases on expressions
--
-- It's likely factored badly.
data ExprTestCase = ExprTestCase
  { -- Parsing tests
   _parsesFrom :: Text -- ^ And also parses from this textual representation
  ,_parsesTo   :: ExprFor CommentedPhase

   -- Resolution tests
  , _underResolveCtx :: ResolveCtx
  , _resolvesTo      :: Expr

  -- Type-checking tests
  ,_underTypeCheckCtx :: TypeCheckCtx -- ^ Under this given typing context
  ,_typed             :: Type -- ^ Has this type

  -- Reduction tests
  ,_underReductionCtx    :: ReductionCtx
  ,_reducesTo            :: Expr                -- ^ Expr reduces to this form. E.G. when it contains lambdas applied to expressions.
  ,_reducesToWhenApplied :: [ReductionTestCase]

  -- Evaluation tests
  ,_underEvaluationCtx     :: EvaluationCtx
  ,_evaluatesTo            :: Expr
  ,_evaluatesToWhenApplied :: [EvaluationTestCase]
  }

-- A Reduction test has a name, a list of transformations and is expected to
-- fail or succeed with some reduced expression.
type ReductionTestCase = (Text, [Expr -> Expr], Maybe Expr)

-- |n Evaluation test has a name, a list of transformations and is expected to
-- fail or succeed with some evaluated expression.
type EvaluationTestCase = (Text, [Expr -> Expr], Maybe Expr)
