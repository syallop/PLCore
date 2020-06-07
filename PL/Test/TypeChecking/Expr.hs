{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.TypeChecking.Expr
  ( typeChecksSpec
  , typeCheckSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Commented
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.TypeCheck
import PL.FixPhase
import PL.Var
import PL.Bindings
import PL.Pattern

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

-- | Test that for each test case, whether an expression typechecks to the intended type.
typeChecksSpec
  :: Map.Map Text.Text ExprTestCase
  -> PPError DefaultPhase
  -> Spec
typeChecksSpec testCases pp
  = describe "All example programs"
  . mapM_ (\(name,testCase)
            -> typeCheckSpec name
                             (_resolvesTo testCase)
                             (_underTypeCheckCtx testCase)
                             (_typed testCase)
                             pp
          )
  . Map.toList
  $ testCases

-- | Test whether an expression typechecks to the intended type.
typeCheckSpec
  :: Text.Text
  -> Expr
  -> TypeCheckCtx
  -> Type
  -> PPError DefaultPhase
  -> Spec
typeCheckSpec name inputExpr ctx expectedType pp = it (Text.unpack name) $
  let exprTy :: Either Error Type
      exprTy = exprType ctx inputExpr
   in case exprTy of
  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

  Right resultType
    -> case checkEqual resultType expectedType ctx of
         Left err
           -> expectationFailure . Text.unpack . render $ text "A given type name does not exist in the context"

         Right False
           -> expectationFailure . Text.unpack . render $ text "Expected: " <> _ppType pp expectedType <> text " got: " <> _ppType pp resultType

         Right True
           -> return ()

