{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.TypeChecking.Pattern
  ( typeChecksPatternsSpec
  , typeCheckPatternSpec
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
import PL.Pattern

import PL.Test.PatternTestCase

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


typeChecksPatternsSpec
  :: Map.Map Text.Text PatternTestCase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
typeChecksPatternsSpec testCases ppType ppError =
  describe "All example patterns"
  . mapM_ (\(name,testCase) -> typeCheckPatternSpec name (_isPattern testCase) (_underTypeCtx testCase) (_typed testCase) ppType ppError)
  . Map.toList
  $ testCases

-- TODO: Decide what it means to type-check a pattern
-- Either:
-- - This is meaningless and should be removed
-- - A pattern is typed like a lambda, but just has the side effect of pattern maching and binding things
-- - A pattern requires a unique type. This might be interesting for passing around patterns as first class things.

typeCheckPatternSpec
  :: Text.Text
  -> PatternFor CommentedPhase
  -> TypeCtx phase
  -> TypeFor phase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
typeCheckPatternSpec name inputPattern underTypeCtx expectedType ppType ppError = it (Text.unpack name) $ pendingWith "There is no defined notion of typechecking for a pattern (yet)"

