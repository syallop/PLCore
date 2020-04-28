{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PL.Test.MatchArgSpec where

import PL
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Var

import PL.Test.MatchArgTestCase
import PL.Test.Parsing.MatchArg
import PL.Test.MatchArg
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
  describe "MatchArgs" $ do
    describe "Type check" $ typeChecksMatchArgsSpec matchArgTestCases ppType (ppError ppMatchArg ppType)
    describe "Reduce"     $ reducesMatchArgsToSpec  matchArgTestCases ppType ppMatchArg
  where
    matchArgTestCases :: Map.Map Text MatchArgTestCase
    matchArgTestCases = mkMatchArgTestCases $ TestMatchArgSources {}

    ppExpr :: ExprFor DefaultPhase -> Doc
    ppExpr = text . Text.pack . show

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = text . Text.pack . show

    ppMatchArg :: MatchArgFor DefaultPhase -> Doc
    ppMatchArg = text . Text.pack . show

