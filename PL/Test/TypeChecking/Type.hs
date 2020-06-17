{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.TypeChecking.Type
  ( typeChecksTypesSpec
  , typeCheckTypeSpec
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
import PL.FixPhase
import PL.Name
import PL.Pattern
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import PL.Var
import PL.Bindings
import PL.ReduceType

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
  -> PPError DefaultPhase
  -> Spec
typeChecksTypesSpec testCases pp =
  describe "All example types"
  . mapM_ (\(name,testCase)
            -> typeCheckTypeSpec name
                                 (_resolvesTo testCase)
                                 (_typeBindCtx    . _underTypeCheckCtx $ testCase)
                                 (_contentHasKind . _underTypeCheckCtx $ testCase)
                                 (_selfKind       . _underTypeCheckCtx $ testCase)
                                 (_typeCtx        . _underTypeCheckCtx $ testCase)
                                 (_typeBindings   . _underTypeCheckCtx $ testCase)
                                 (_hasKind testCase)
                                 pp
          )
  . Map.toList
  $ testCases

-- | Test whether a type typechecks (kind checks) to the intended kind.
typeCheckTypeSpec
  :: Text.Text
  -> Type
  -> BindCtx TyVar Kind
  -> Map.Map ContentName Kind
  -> Maybe Kind
  -> TypeCtx
  -> Bindings Type
  -> Kind
  -> PPError DefaultPhase
  -> Spec
typeCheckTypeSpec name inputType bindCtx contentHasKind mSelfKind underTypeCtx bindings expectedKind pp = it (Text.unpack name) $ case typeKind bindCtx contentHasKind mSelfKind underTypeCtx inputType of
  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

  Right resultKind
    | not (kindEq resultKind expectedKind)
     -> expectationFailure . Text.unpack . render $ text "Expected:" <> _ppKind pp expectedKind <> text " got:" <> _ppKind pp resultKind

    | otherwise
    -> pure ()

