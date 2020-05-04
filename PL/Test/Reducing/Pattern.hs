{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , GADTs
  , RankNTypes
  #-}
module PL.Test.Reducing.Pattern
  ( reducesPatternsToSpec
  , reducePatternToSpec
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
import PL.Pattern
import PL.Var
import PL.Bindings

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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util

-- | Test each pattern reduces to expected results.
reducesPatternsToSpec
  :: Map.Map Text.Text PatternTestCase
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> (Var -> Doc)
  -> (TyVar -> Doc)
  -> Spec
reducesPatternsToSpec testCases ppExpr ppType ppPattern ppVar ppTyVar =
  describe "All example patterns"
    . mapM_ (\(name,testCase)
              -> reducePatternToSpec name
                                     (_underTypeCtx testCase)
                                     (_underExprBindCtx testCase)
                                     (_underTypeBindCtx testCase)
                                     (_underTypeBindings testCase)
                                     (stripPatternComments $ _isPattern testCase)
                                     (_typed testCase)
                                     (_checkMatchWithResult testCase)
                                     ppExpr
                                     ppType
                                     ppPattern
                                     ppVar
                                     ppTyVar
            )
    . Map.toList
    $ testCases

-- | Test whether a pattern reduces to bind the expected values.
reducePatternToSpec
  :: Text.Text
  -> TypeCtx DefaultPhase
  -> BindCtx (BindingFor DefaultPhase) (AbstractionFor DefaultPhase)
  -> BindCtx (TypeBindingFor DefaultPhase) Kind
  -> Bindings (TypeFor DefaultPhase)
  -> PatternFor DefaultPhase
  -> Type
  -> Either (Error (ExprFor DefaultPhase) (TypeFor DefaultPhase) (PatternFor DefaultPhase)) [TypeFor DefaultPhase]
  -> (ExprFor DefaultPhase -> Doc)
  -> (TypeFor DefaultPhase -> Doc)
  -> (PatternFor DefaultPhase -> Doc)
  -> (Var -> Doc)
  -> (TyVar -> Doc)
  -> Spec
reducePatternToSpec name typeCtx exprBindCtx typeBindCtx typeBindings testPattern expectTy expect ppExpr ppType ppPattern ppVar ppTyVar =
  it (Text.unpack name) $ isExpected (checkWithPattern testPattern expectTy exprBindCtx typeBindCtx typeBindings typeCtx) expect
  where
    isExpected
      :: Either (Error Expr Type Pattern) [TypeFor DefaultPhase]
      -> Either (Error Expr Type Pattern) [TypeFor DefaultPhase]
      -> Expectation
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ text "Pattern expected error:"
            , ppError ppPattern ppType ppExpr ppVar ppTyVar expectedErr
            , text "but got:"
            , ppError ppPattern ppType ppExpr ppVar ppTyVar resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromRight False . uncurry (typeEq typeBindCtx typeBindings typeCtx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ text "Pattern expected to bind:"
               , foldr ((<>) . ppType) mempty expectedTys
               , text "but bound:"
               , foldr ((<>) . ppType) mempty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "Pattern expected error:"
             , ppError ppPattern ppType ppExpr ppVar ppTyVar expectedErr
             , text "but got successful result, binding types:"
             , foldr ((<>) . ppType) mempty resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "Pattern expected to bind:"
             , foldr ((<>) . ppType) mempty expectedTys
             , text "but got error:"
             , ppError ppPattern ppType ppExpr ppVar ppTyVar resultErr
             ]

    fromRight :: b -> Either a b -> b
    fromRight _ (Right b) = b
    fromRight b _         = b

