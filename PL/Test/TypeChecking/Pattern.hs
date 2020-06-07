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
import PL.FixPhase
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
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
  -> PPError DefaultPhase
  -> Spec
typeChecksPatternsSpec testCases pp =
  describe "All example patterns"
  . mapM_ (\(name,testCase)
            -> typeCheckPatternSpec name
                                    (_resolvesTo testCase)
                                    (_typeCtx . _underTypeCheckCtx $ testCase)
                                    (_typed testCase)
                                    pp
          )
  . Map.toList
  $ testCases

-- TODO: Decide what it means to type-check a pattern
-- Either:
-- - This is meaningless and should be removed
-- - A pattern is typed like a lambda, but just has the side effect of pattern maching and binding things
-- - A pattern requires a unique type. This might be interesting for passing around patterns as first class things.

typeCheckPatternSpec
  :: Text.Text
  -> Pattern
  -> TypeCtx
  -> Type
  -> PPError DefaultPhase
  -> Spec
typeCheckPatternSpec name inputPattern underTypeCtx expectedType pp = it (Text.unpack name) $ pendingWith "There is no defined notion of typechecking for a pattern (yet)"

