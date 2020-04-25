{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PL.Test.TypeSpec where

import PL
import PL.Case
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Var

import PL.Test.TypeTestCase
import PL.Test.Parsing.Type
import PL.Test.Type
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
  describe "Types" $ do
    describe "Type (kind) check" $ typeChecksTypesSpec typeTestCases ppKind (ppError ppType)
    describe "Reduce"            $ reducesTypesToSpec  typeTestCases ppType
  where
    typeTestCases :: Map.Map Text TypeTestCase
    typeTestCases = mkTypeTestCases $ TestTypeSources {}

    ppExpr :: ExprFor DefaultPhase -> Doc
    ppExpr = text . Text.pack . show

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = text . Text.pack . show

    ppKind :: Kind -> Doc
    ppKind = text . Text.pack . show
