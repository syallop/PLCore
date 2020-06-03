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
  -> (Source -> Either (Error Expr Type Pattern TypeCtx) (PatternFor CommentedPhase, Source))
  -> (Pattern -> Doc)
  -> (Error Expr Type Pattern TypeCtx -> Doc)
  -> Spec
parsesToPatternsSpec testCases parsePattern ppPattern ppError
  = describe "All example patterns"
  . mapM_ (\(name,testCase) -> parseToPatternSpec parsePattern name (_parsesFrom testCase) (_isPattern testCase) ppPattern ppError)
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended pattern.
parseToPatternSpec
  :: (Source -> Either (Error Expr Type Pattern TypeCtx) (PatternFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> PatternFor CommentedPhase
  -> (Pattern -> Doc)
  -> (Error Expr Type Pattern TypeCtx -> Doc)
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

    -- | TODO:
    -- Several hacks are colliding here. Unpick them:
    -- - Stored patterns are used as examples.
    -- - This means they're commented.
    -- - Comments are not currently transfered to the test-cases.
    -- This means we're comparing with the comments stripped. The tradeoff is we
    -- cant test comment parsing.
    | otherwise
     -> let parsedWithoutComments   = stripPatternComments parsedPattern
            expectedWithoutComments = stripPatternComments expectedPattern
         in if parsedWithoutComments == expectedWithoutComments
              then pure ()
              else expectationFailure . Text.unpack . render . document . mconcat $
                     [ text "Successfully parsed without leftovers an unexpected pattern. Got:"
                     , lineBreak
                     , indent1 . ppPattern $ parsedWithoutComments
                     , lineBreak
                     , text "But we expected:"
                     , lineBreak
                     , indent1 . ppPattern $ expectedWithoutComments
                     ]

    | otherwise
     -> pure ()

