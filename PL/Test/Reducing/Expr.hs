{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  #-}
module PL.Test.Reducing.Expr
  ( reducesToSpec
  , reduceToSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.Bindings

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
  -> Spec
reducesToSpec testCases ppExpr ppType =
  describe "All example programs reduce as expected"
    . mapM_ (\(name,testCase) -> reduceToSpec name (_isExpr testCase) (("Reduces",[],_reducesTo testCase) : _reducesToWhenApplied testCase) ppExpr ppType)
    . Map.toList
    $ testCases

-- | Test whether an expression reduces to another.
--
-- Name an expression, apply it to a list of (argnames,argument,expected result) tuples.
-- Where the expression in turn applied to each list of arguments must reduce to the given expected result
reduceToSpec
  :: phase ~ DefaultPhase
  => Text.Text
  -> ExprFor DefaultPhase
  -> [(Text.Text, [ExprFor DefaultPhase], ExprFor DefaultPhase)]
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> Spec
reduceToSpec name inputExpr reductions ppExpr ppType = describe (Text.unpack name <> " reduces as expected") $
  mapM_ (\(name,args,expectReduction) -> reduceSpec name (appise (inputExpr:args)) expectReduction ppExpr ppType) reductions
  where
    reduceSpec
      :: Text.Text
      -> Expr
      -> Expr
      -> (Expr -> Doc)
      -> (Type -> Doc)
      -> Spec
    reduceSpec name expr eqExpr ppExpr ppType = it (Text.unpack name) $ case reduce expr of
      Left exprErr
        -> expectationFailure . Text.unpack . render . ppError ppType $ exprErr

      Right redExpr
        -> if redExpr == eqExpr
             then return ()

             -- Doesnt equal initial expression.
             -- reduce that expression and check once more
             else case reduce eqExpr of
                    Left eqExprErr
                      -> expectationFailure . Text.unpack . render $ mconcat
                           [ text "target expression reduces, doesnt match the expected expression AND the expected expression fails to reduce itself:"
                           , ppError ppType eqExprErr
                           ]

                    Right redEqExpr
                      -> if redExpr == redEqExpr
                           then return ()
                           else expectationFailure "target and expected expression both reduce BUT they are not equal"
