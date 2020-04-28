{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.TypeChecking.Expr
  ( typeChecksSpec
  , typeCheckSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Commented
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
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- | Test that for each test case, whether an expression typechecks to the intended type.
typeChecksSpec
  :: Map.Map Text.Text ExprTestCase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type MatchArg -> Doc)
  -> Spec
typeChecksSpec testCases ppType ppError
  = describe "All example programs type check as expected"
  . mapM_ (\(name,testCase) -> typeCheckSpec name (_isExpr testCase) (_underTypeCtx testCase) (_typed testCase) ppType ppError)
  . Map.toList
  $ testCases

-- | Test whether an expression typechecks to the intended type.
typeCheckSpec
  :: Text.Text
  -> ExprFor CommentedPhase
  -> TypeCtx DefaultPhase
  -> TypeFor DefaultPhase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type MatchArg -> Doc)
  -> Spec
typeCheckSpec name inputExpr underTypeCtx expectedType ppType ppError = it (Text.unpack name <> " type checks as expected") $ case topExprType underTypeCtx (stripComments inputExpr) of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  Right resultType
    -> case typeEq emptyCtx emptyBindings underTypeCtx resultType expectedType of
         Left err
           -> expectationFailure . Text.unpack . render $ text "A given type name does not exist in the context"

         Right False
           -> expectationFailure . Text.unpack . render $ text "Expected: " <> ppType expectedType <> text " got: " <> ppType resultType

         Right True
           -> return ()

