{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  #-}
module PL.Test.Parsing.Type
  ( parsesToTypesSpec
  , parseToTypeSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
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

-- | Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended type.
parsesToTypesSpec
  :: Map.Map Text.Text TypeTestCase
  -> (Source -> Either (Error Type Pattern) (TypeFor CommentedPhase, Source))
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
parsesToTypesSpec testCases parseType ppType ppError
  = describe "All example types can be parsed by some parser and some source"
  . mapM_ (\(name,testCase) -> parseToTypeSpec parseType name (_parsesFrom testCase) (_isType testCase) ppType ppError)
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended type.
parseToTypeSpec
  :: (Source -> Either (Error Type Pattern) (TypeFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> TypeFor CommentedPhase
  -> (TypeFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
parseToTypeSpec parseType name inputSource expectedType ppType ppError = it (Text.unpack name <> " can be parsed by some parser and some source") $ case parseType inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  -- No leftovers are allowed and the parsed expression must equal the expected
  -- type.
  Right (parsedType, leftovers)
    | not (Text.null leftovers)
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Unexpected leftovers:"
         , lineBreak
         , indent1 $ text leftovers
         , lineBreak
         , text "After parsing:"
         , lineBreak
         , indent1 . ppType . stripTypeComments $ parsedType
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 . ppType . stripTypeComments $ expectedType
         ]

    | parsedType /= expectedType
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Successfully parsed without leftovers an unexpected type. Got:"
         , lineBreak
         , indent1 . ppType . stripTypeComments $ parsedType
         , lineBreak
         , text "But we expected:"
         , lineBreak
         , indent1 . ppType . stripTypeComments $ expectedType
         ]

    | otherwise
     -> pure ()

