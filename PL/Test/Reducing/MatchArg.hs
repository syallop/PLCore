{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.Reducing.MatchArg
  ( reducesMatchArgsToSpec
  , reduceMatchArgToSpec
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- | Test each matcharg reduces to expected results.
reducesMatchArgsToSpec
  :: Map.Map Text.Text MatchArgTestCase
  -> (TypeFor DefaultPhase -> Doc)
  -> Spec
reducesMatchArgsToSpec testCases ppType =
  describe "All example matcharg programs reduce as expected"
    . mapM_ (\(name,testCase) -> reduceMatchArgToSpec name (_underTypeCtx testCase) (_underExprBindCtx testCase) (_underTypeBindCtx testCase) (_underTypeBindings testCase) (stripMatchArgComments $ _isMatchArg testCase) (_typed testCase) (_checkMatchWithResult testCase) ppType)
    . Map.toList
    $ testCases

-- | Test whether a matcharg reduces to bind the expected values.
reduceMatchArgToSpec
  :: forall phase
   . phase ~ DefaultPhase
  => Text.Text
  -> TypeCtx phase
  -> BindCtx (BindingFor phase) (AbstractionFor phase)
  -> BindCtx (TypeBindingFor phase) Kind
  -> Bindings (TypeFor phase)
  -> MatchArgFor phase
  -> Type
  -> Either (Error phase) [TypeFor phase]
  -> (TypeFor DefaultPhase -> Doc)
  -> Spec
reduceMatchArgToSpec name typeCtx exprBindCtx typeBindCtx typeBindings testMatchArg expectTy expect ppType =
  it (Text.unpack name <> " reduces as expected") $ isExpected (checkMatchWith testMatchArg expectTy exprBindCtx typeBindCtx typeBindings typeCtx) expect
  where
  {-
    isExpected
      :: Either (Error phase) [TypeFor phase]
      -> Either (Error phase) [TypeFor phase]
      -> Expectation
  -}
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ text "MatchArg expected error:"
            , ppError ppType expectedErr
            , text "but got:"
            , ppError ppType resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromRight False . uncurry (typeEq typeBindCtx typeBindings typeCtx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ text "MatchArg expected to bind:"
               , foldr ((<>) . ppType) mempty expectedTys
               , text "but bound:"
               , foldr ((<>) . ppType) mempty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected error:"
             , ppError ppType expectedErr
             , text "but got successful result, binding types:"
             , foldr ((<>) . ppType) mempty resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "MatchArg expected to bind:"
             , foldr ((<>) . ppType) mempty expectedTys
             , text "but got error:"
             , ppError ppType resultErr
             ]

    fromRight :: b -> Either a b -> b
    fromRight _ (Right b) = b
    fromRight b _         = b

