{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.TypeChecking.MatchArg
  ( typeChecksMatchArgsSpec
  , typeCheckMatchArgSpec
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
import PL.Var
import PL.Bindings

import PL.Test.MatchArgTestCase

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

-- TODO: We might need to expose core functions for this. Do match expressions
-- themselves currently have a type? If they did we might be able to pass them around
-- first class more easily.


typeChecksMatchArgsSpec
  :: Map.Map Text.Text MatchArgTestCase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error DefaultPhase -> Doc)
  -> Spec
typeChecksMatchArgsSpec testCases ppType ppError =
  describe "All example matchargs type check"
  . mapM_ (\(name,testCase) -> typeCheckMatchArgSpec name (_isMatchArg testCase) (_underTypeCtx testCase) (_typed testCase) ppType ppError)
  . Map.toList
  $ testCases

-- TODO: Decide what it means to type-check a matcharg
-- Either:
-- - This is meaningless and should be removed
-- - A matcharg is typed like a lambda, but just has the side effect of pattern maching and binding things
-- - A matcharg requires a unique type. This might be interesting for passing around patterns as first class things.

typeCheckMatchArgSpec
  :: Text.Text
  -> MatchArgFor CommentedPhase
  -> TypeCtx phase
  -> TypeFor phase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error DefaultPhase -> Doc)
  -> Spec
typeCheckMatchArgSpec name inputMatchArg underTypeCtx expectedType ppType ppError = it (Text.unpack name <> " type checks as expected") $ pendingWith "There is no defined notion of typechecking for a matcharg (yet)"

