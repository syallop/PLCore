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

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
import PL.Var
import PL.Bindings
import PL.Pattern

import PL.Test.ExprTestCase

import PLGrammar
import PLPrinter
import PLPrinter.Doc

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding (Product,Sum)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- | Test each testcase reduces to expected results.
reducesToSpec
  :: Map.Map Text.Text ExprTestCase
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> (Var -> Doc)
  -> (TyVar -> Doc)
  -> Spec
reducesToSpec testCases ppExpr ppType ppPattern ppVar ppTyVar =
  describe "All example programs"
    . mapM_ (\(name,testCase)
              -> reduceToSpec name
                              (_underReductionCtx testCase)
                              (_resolvesTo testCase)
                              (("Reduces",[],Just . _reducesTo $ testCase) : _reducesToWhenApplied testCase)
                              ppExpr
                              ppType
                              ppPattern
                              ppVar
                              ppTyVar
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
  -> (Expr -> Doc)
  -> (Type -> Doc)
  -> (Pattern -> Doc)
  -> (Var -> Doc)
  -> (TyVar -> Doc)
  -> Spec
reduceToSpec name ctx inputExpr reductions ppExpr ppType ppPattern ppVar ppTyVar = describe (Text.unpack name) $
  mapM_ (\(name,args,expectReduction)
          -> reduceSpec name (apply inputExpr args) expectReduction
        )
        reductions
  where
    reduceSpec
      :: Text.Text
      -> ExprFor DefaultPhase
      -> Maybe (ExprFor DefaultPhase)
      -> Spec
    reduceSpec name expr eqExpr = it (Text.unpack name) $ case (reduce ctx expr, eqExpr) of
      (Left exprErr, Just expectedExpr)
        -> expectationFailure . Text.unpack . render $ mconcat
             [ text "Could not reduce:"
             , lineBreak
             , indent1 $ ppExpr expr
             , lineBreak
             , text "With error:"
             , lineBreak
             , ppError ppPattern ppType ppExpr (ppTypeCtx document (ppTypeInfo ppType)) ppVar ppTyVar $ exprErr
             , lineBreak
             , text "Expected:"
             , ppExpr expectedExpr
             ]

      -- We expected _some_ error and got _some_.
      (Left _exprErr, Nothing)
        -> pure ()

      (Right redExpr, Nothing)
        -> expectationFailure . Text.unpack . render $ mconcat
             [ text "Expression reduced to:"
             , lineBreak
             , indent1 $ ppExpr redExpr
             , lineBreak
             , text "But we expected the reducation to fail."
             ]

      (Right redExpr, Just expectedExpr)
        -> case exprEq ctx redExpr expectedExpr of
             Left err
               -> expectationFailure . Text.unpack . render $ mconcat
                    [ text "The expression reduced to:"
                    , lineBreak
                    , indent1 $ ppExpr redExpr
                    , lineBreak
                    , text "But failed to compare to the expected expression:"
                    , lineBreak
                    , indent1 $ ppExpr expectedExpr
                    , lineBreak
                    , text "Due to error:"
                    , lineBreak
                    , indent1 $ ppError ppPattern ppType ppExpr (ppTypeCtx document (ppTypeInfo ppType)) ppVar ppTyVar err
                    ]

             Right False
               -> expectationFailure . Text.unpack . render . mconcat $
                    [ text "The expression reduced to:"
                    , lineBreak
                    , indent1 $ ppExpr redExpr
                    , lineBreak
                    , text "But does not equal the expected expression:"
                    , lineBreak
                    , indent1 $ ppExpr expectedExpr
                    ]

             Right True
               -> pure ()

apply :: ExprFor phase -> [ExprFor phase -> ExprFor phase] -> ExprFor phase
apply e fs = foldl (\e f -> f e) e fs

