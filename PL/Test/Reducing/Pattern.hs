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
import PL.TypeCheck
import PL.FixPhase
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
  -> PPError DefaultPhase
  -> Spec
reducesPatternsToSpec testCases pp =
  describe "All example patterns"
    . mapM_ (\(name,testCase)
              -> reducePatternToSpec name
                                     (_underTypeCheckCtx testCase)
                                     (_resolvesTo testCase)
                                     (_typed testCase)
                                     (_bindsOnMatch testCase)
                                     pp
            )
    . Map.toList
    $ testCases

-- | Test whether a pattern reduces to bind the expected values.
reducePatternToSpec
  :: Text.Text
  -> TypeCheckCtx
  -> Pattern
  -> Type
  -> Either Error [Type]
  -> PPError DefaultPhase
  -> Spec
reducePatternToSpec name ctx testPattern expectTy expect pp =
  it (Text.unpack name) $ isExpected (checkWithPattern testPattern expectTy ctx) expect
  where
    isExpected
      :: Either Error [Type]
      -> Either Error [Type]
      -> Expectation
    isExpected result expected = case (result,expected) of
      (Left resultErr, Left expectedErr)
        | resultErr == expectedErr -> return ()
        | otherwise  -> expectationFailure $ Text.unpack $ render $ mconcat
            [ text "Pattern expected error:"
            , indent1 . ppError pp $ expectedErr
            , text "but got:"
            , indent1 . ppError pp $ resultErr
            ]

      (Right resultTys, Right expectedTys)
        | length resultTys == length expectedTys
          && all (fromRight False . uncurry (\a b -> checkEqual a b ctx)) (zip resultTys expectedTys)
          -> return ()

        | otherwise
          -> expectationFailure $ Text.unpack $ render $ mconcat
               [ text "Pattern expected to bind:"
               , foldr ((<>) . _ppType pp) mempty expectedTys
               , text "but bound:"
               , foldr ((<>) . _ppType pp) mempty resultTys
               ]

      (Right resultTys, Left expectedErr)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "Pattern expected error:"
             , indent1 . ppError pp $ expectedErr
             , text "but got successful result, binding types:"
             , indent1 . foldr ((<>) . _ppType pp) mempty $ resultTys
             ]

      (Left resultErr, Right expectedTys)
        -> expectationFailure $ Text.unpack $ render $ mconcat
             [ text "Pattern expected to bind:"
             , indent1 . foldr ((<>) . _ppType pp) mempty $ expectedTys
             , text "but got error:"
             , indent1 . ppError pp $ resultErr
             ]

    fromRight :: b -> Either a b -> b
    fromRight _ (Right b) = b
    fromRight b _         = b

