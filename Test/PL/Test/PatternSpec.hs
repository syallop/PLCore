{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PL.Test.PatternSpec where

import PL
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Var
import PL.Pattern
import PL.TypeCtx

import PL.Test.PatternTestCase
import PL.Test.Parsing.Pattern
import PL.Test.Pattern
import PL.Test.Source

import PLGrammar
import PLPrinter
import PLPrinter.Doc

import Control.Monad
import Data.Text
import qualified Data.Text as Text
import Data.Monoid hiding (Product, Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.List as List

import Test.Hspec

spec
  :: Spec
spec = do
  describe "Patterns" $ do
    describe "Type check" $ typeChecksPatternsSpec patternTestCases ppType (ppError ppPattern ppType ppExpr (ppTypeCtx document (ppTypeInfo ppType)) ppVar ppTyVar)
    describe "Reduce"     $ reducesPatternsToSpec  patternTestCases ppExpr ppType ppPattern ppVar ppTyVar
  where
    patternTestCases :: Map.Map Text PatternTestCase
    patternTestCases = mkPatternTestCases $ TestPatternSources {}

    ppExpr :: ExprFor DefaultPhase -> Doc
    ppExpr = text . Text.pack . show

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = text . Text.pack . show

    ppPattern :: PatternFor DefaultPhase -> Doc
    ppPattern = text . Text.pack . show

    ppVar :: Var -> Doc
    ppVar = text . Text.pack . show

    ppTyVar :: TyVar -> Doc
    ppTyVar = text . Text.pack . show

