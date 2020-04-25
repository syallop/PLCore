{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module PL.Test.Parsing.Expr
  ( parsesToSpec
  , parseToSpec
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
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended expression.
parsesToSpec
  :: Map.Map Text.Text ExprTestCase
  -> (Source -> Either (Error DefaultPhase) (ExprFor DefaultPhase, Source))
  -> (ExprFor DefaultPhase -> Doc)
  -> (Error DefaultPhase -> Doc)
  -> Spec
parsesToSpec testCases parseExpression ppExpr ppError
  = describe "All example programs can be parsed by some parser and some source"
  . mapM_ (\(name,testCase) -> parseToSpec parseExpression name (_parsesFrom testCase) (_isExpr testCase) ppExpr ppError)
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended expression.
parseToSpec
  :: (Eq (ExprFor phase))
  => (Source -> Either (Error phase) (ExprFor phase,Source))
  -> Text.Text
  -> Source
  -> ExprFor phase
  -> (ExprFor phase -> Doc)
  -> (Error phase -> Doc)
  -> Spec
parseToSpec parseExpression name inputSource expectedExpr ppExpr ppError = it (Text.unpack name <> " can be parsed by some parser and some source") $ case parseExpression inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  -- No leftovers are allowed and the parsed expression must equal the expected
  -- expression.
  Right (parsedExpr, leftovers)
    | not (Text.null leftovers)
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Unexpected leftovers:"
         , lineBreak
         , indent1 $ text leftovers
         , lineBreak
         , text "After parsing:"
         , lineBreak
         , indent1 $ ppExpr parsedExpr
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 $ ppExpr expectedExpr
         ]

    | parsedExpr /= expectedExpr
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Successfully parsed without leftovers an unexpected expression. Got:"
         , lineBreak
         , indent1 $ ppExpr parsedExpr
         , lineBreak
         , text "But we expected:"
         , lineBreak
         , indent1 $ ppExpr expectedExpr
         ]

    | otherwise
     -> pure ()


