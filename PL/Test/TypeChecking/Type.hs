{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  #-}
module PL.Test.TypeChecking.Type
  ( typeChecksTypesSpec
  , typeCheckTypeSpec
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

import PL.Test.TypeTestCase

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

-- | Test that for each test case, a type typechecks (kind checks) to the expected
-- kind.
typeChecksTypesSpec
  :: Map.Map Text.Text TypeTestCase
  -> (Kind -> Doc)
  -> (Error DefaultPhase -> Doc)
  -> Spec
typeChecksTypesSpec testCases ppKind ppError =
  describe "All example types type(kind) check"
  . mapM_ (\(name,testCase) -> typeCheckTypeSpec name (_isType testCase) (_underTypeBindCtx testCase) (_underTypeCtx testCase) (_hasKind testCase) ppKind ppError)
  . Map.toList
  $ testCases

-- | Test whether a type typechecks (kind checks) to the intended kind.
typeCheckTypeSpec
  :: phase ~ DefaultPhase
  => Text.Text
  -> TypeFor phase
  -> BindCtx (TypeBindingFor phase) Kind
  -> TypeCtx phase
  -> Kind
  -> (Kind -> Doc)
  -> (Error phase -> Doc)
  -> Spec
typeCheckTypeSpec name inputType bindCtx underTypeCtx expectedKind ppKind ppError = it (Text.unpack name <> " kind checks as expected") $ case typeKind bindCtx underTypeCtx inputType of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  Right resultKind
    | not (kindEq resultKind expectedKind)
     -> expectationFailure . Text.unpack . render $ text "Expected:" <> ppKind expectedKind <> text " got:" <> ppKind resultKind

    | otherwise
    -> pure ()
