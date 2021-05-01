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
import PL.Error
import PL.Kind
import PL.TyVar
import PL.Type
import PL.FixPhase
import PL.Name
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import PL.Bindings

import PL.Test.TypeTestCase

import PLPrinter

import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec

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
typeCheckTypeSpec name inputType bindCtx contentHasKind mSelfKind underTypeCtx _bindings expectedKind pp = it (Text.unpack name) $ case typeKind bindCtx contentHasKind mSelfKind underTypeCtx inputType of
  -- TODO: Why are we passing unused bindings here?

  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

  Right resultKind
    | not (kindEq resultKind expectedKind)
     -> expectationFailure . Text.unpack . render $ text "Expected:" <> _ppKind pp expectedKind <> text " got:" <> _ppKind pp resultKind

    | otherwise
    -> pure ()

