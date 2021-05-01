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

import PL.Error
import PL.Expr
import PL.Type
import PL.TypeCheck
import PL.FixPhase

import PL.Test.ExprTestCase

import PLPrinter

import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec

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
           -> expectationFailure . Text.unpack . render $ ppError pp err

         Right False
           -> expectationFailure . Text.unpack . render $ text "Expected: " <> _ppType pp expectedType <> text " got: " <> _ppType pp resultType

         Right True
           -> return ()

