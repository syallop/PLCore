{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  #-}
module PL.Test.Parsing.Pattern
  ( parsesToPatternsSpec
  , parseToPatternSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Expr
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.Pattern
import PL.TypeCtx
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
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List
import Data.Text (Text)

import Test.Hspec
import PL.Test.Source
import PL.Test.Util


-- Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended Pattern.
parsesToPatternsSpec
  :: Map.Map Text.Text PatternTestCase
  -> (Source -> Either (Error Type Pattern) (PatternFor CommentedPhase, Source))
  -> (PatternFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
parsesToPatternsSpec testCases parsePattern ppPattern ppError
  = describe "All example patterns"
  . mapM_ (\(name,testCase) -> parseToPatternSpec parsePattern name (_parsesFrom testCase) (_isPattern testCase) ppPattern ppError)
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended pattern.
parseToPatternSpec
  :: (Source -> Either (Error Type Pattern) (PatternFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> PatternFor CommentedPhase
  -> (PatternFor DefaultPhase -> Doc)
  -> (Error Type Pattern -> Doc)
  -> Spec
parseToPatternSpec parsePattern name inputSource expectedPattern ppPattern ppError = it (Text.unpack name) $ case parsePattern inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  -- No leftovers are allowed and the parsed expression must equal the expected
  -- pattern.
  Right (parsedPattern, leftovers)
    | not (Text.null leftovers)
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Unexpected leftovers:"
         , lineBreak
         , indent1 $ text leftovers
         , lineBreak
         , text "After parsing:"
         , lineBreak
         , indent1 . ppPattern . stripPatternComments $ parsedPattern
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 . ppPattern . stripPatternComments $ expectedPattern
         ]

    | parsedPattern /= expectedPattern
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Successfully parsed without leftovers an unexpected pattern. Got:"
         , lineBreak
         , indent1 . ppPattern . stripPatternComments $ parsedPattern
         , lineBreak
         , text "But we expected:"
         , lineBreak
         , indent1 . ppPattern .stripPatternComments $ expectedPattern
         ]

    | otherwise
     -> pure ()

