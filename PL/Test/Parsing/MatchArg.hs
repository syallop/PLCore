{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}
module PL.Test.Parsing.MatchArg
  ( parsesToMatchArgsSpec
  , parseToMatchArgSpec
  )
  where

import PL.Binds
import PL.Case
import PL.Error
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Expr
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


-- Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended MatchArg.
parsesToMatchArgsSpec
  :: Map.Map Text.Text MatchArgTestCase
  -> (Source -> Either (Error DefaultPhase) (MatchArgFor DefaultPhase, Source))
  -> (MatchArgFor DefaultPhase -> Doc)
  -> (Error DefaultPhase -> Doc)
  -> Spec
parsesToMatchArgsSpec testCases parseMatchArg ppMatchArg ppError
  = describe "All example matchargs can be parsed by some parser and some source"
  . mapM_ (\(name,testCase) -> parseToMatchArgSpec parseMatchArg name (_parsesFrom testCase) (_isMatchArg testCase) ppMatchArg ppError)
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended matcharg.
parseToMatchArgSpec
  :: (Eq (MatchArgFor phase))
  => (Source -> Either (Error phase) (MatchArgFor phase,Source))
  -> Text.Text
  -> Source
  -> MatchArgFor phase
  -> (MatchArgFor phase -> Doc)
  -> (Error phase -> Doc)
  -> Spec
parseToMatchArgSpec parseMatchArg name inputSource expectedMatchArg ppMatchArg ppError = it (Text.unpack name <> " can be parsed by some parser and some source") $ case parseMatchArg inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError $ err

  -- No leftovers are allowed and the parsed expression must equal the expected
  -- matcharg.
  Right (parsedMatchArg, leftovers)
    | not (Text.null leftovers)
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Unexpected leftovers:"
         , lineBreak
         , indent1 $ text leftovers
         , lineBreak
         , text "After parsing:"
         , lineBreak
         , indent1 $ ppMatchArg parsedMatchArg
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 $ ppMatchArg expectedMatchArg
         ]

    | parsedMatchArg /= expectedMatchArg
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Successfully parsed without leftovers an unexpected matcharg. Got:"
         , lineBreak
         , indent1 $ ppMatchArg parsedMatchArg
         , lineBreak
         , text "But we expected:"
         , lineBreak
         , indent1 $ ppMatchArg expectedMatchArg
         ]

    | otherwise
     -> pure ()

