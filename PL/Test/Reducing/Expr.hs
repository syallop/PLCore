{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.Reducing.Expr
  ( reducesToSpec
  , reduceToSpec
  )
  where

import PL.Error
import PL.Expr
import PL.Reduce
import PL.FixPhase

import PL.Test.ExprTestCase

import PLPrinter

import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec

-- | Test each testcase reduces to expected results.
reducesToSpec
  :: Map.Map Text.Text ExprTestCase
  -> PPError DefaultPhase
  -> Spec
reducesToSpec testCases pp =
  describe "All example programs"
    . mapM_ (\(name,testCase)
              -> reduceToSpec name
                              (_underReductionCtx testCase)
                              (_resolvesTo testCase)
                              (("Reduces",[],Just . _reducesTo $ testCase) : _reducesToWhenApplied testCase)
                              pp
            )
    . Map.toList
    $ testCases

-- | Test whether an expression reduces to another.
--
-- Name an expression, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the expression in turn applied to each list of arguments must reduce to the given expected result
reduceToSpec
  :: Text.Text
  -> ReductionCtx
  -> Expr
  -> [ReductionTestCase]
  -> PPError DefaultPhase
  -> Spec
reduceToSpec name ctx inputExpr reductions pp = describe (Text.unpack name) $
  mapM_ (\(n,args,expectReduction)
          -> reduceSpec n ctx (apply inputExpr args) expectReduction pp
        )
        reductions

reduceSpec
  :: Text.Text
  -> ReductionCtx
  -> Expr
  -> Maybe Expr
  -> PPError DefaultPhase
  -> Spec
reduceSpec name ctx expr eqExpr pp = it (Text.unpack name) $ case (reduce ctx expr, eqExpr) of
  (Left exprErr, Just expectedExpr)
    -> expectationFailure . Text.unpack . render $ mconcat
         [ text "Could not reduce:"
         , lineBreak
         , indent1 . _ppExpr pp $ expr
         , lineBreak
         , text "With error:"
         , lineBreak
         , indent1 . ppError pp $ exprErr
         , lineBreak
         , text "Expected:"
         , indent1 . _ppExpr pp $ expectedExpr
         ]

  -- We expected _some_ error and got _some_.
  (Left _exprErr, Nothing)
    -> pure ()

  (Right redExpr, Nothing)
    -> expectationFailure . Text.unpack . render $ mconcat
         [ text "Expression reduced to:"
         , lineBreak
         , indent1 . _ppExpr pp $ redExpr
         , lineBreak
         , text "But we expected the reducation to fail."
         ]

  (Right redExpr, Just expectedExpr)
    -> case exprEq ctx redExpr expectedExpr of
         Left err
           -> expectationFailure . Text.unpack . render $ mconcat
                [ text "The expression reduced to:"
                , lineBreak
                , indent1 . _ppExpr pp $ redExpr
                , lineBreak
                , text "But failed to compare to the expected expression:"
                , lineBreak
                , indent1 . _ppExpr pp $ expectedExpr
                , lineBreak
                , text "Due to error:"
                , lineBreak
                , indent1 . ppError pp $ err
                ]

         Right False
           -> expectationFailure . Text.unpack . render . mconcat $
                [ text "The expression reduced to:"
                , lineBreak
                , indent1 . _ppExpr pp $ redExpr
                , lineBreak
                , text "But does not equal the expected expression:"
                , lineBreak
                , indent1 . _ppExpr pp $ expectedExpr
                ]

         Right True
           -> pure ()

apply :: ExprFor phase -> [ExprFor phase -> ExprFor phase] -> ExprFor phase
apply baseExpr fs = foldl (\e f -> f e) baseExpr fs

