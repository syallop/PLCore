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

import PL.Commented
import PL.Error
import PL.FixPhase
import PL.Pattern

import PL.Test.PatternTestCase

import PLPrinter

import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec
import PL.Test.Source

-- Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended Pattern.
parsesToPatternsSpec
  :: Map.Map Text.Text PatternTestCase
  -> (Source -> Either Error (PatternFor CommentedPhase, Source))
  -> (PatternFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parsesToPatternsSpec testCases parsePattern ppPattern pp
  = describe "All example patterns"
  . mapM_ (\(name,testCase)
            -> parseToPatternSpec parsePattern
                                  name
                                  (_parsesFrom testCase)
                                  (_parsesTo testCase)
                                  ppPattern
                                  pp
          )
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended pattern.
parseToPatternSpec
  :: (Source -> Either Error (PatternFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> PatternFor CommentedPhase
  -> (PatternFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parseToPatternSpec parsePattern name inputSource expectedPattern ppPattern pp = it (Text.unpack name) $ case parsePattern inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

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
         , indent1 . ppPattern $ parsedPattern
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 . ppPattern $ expectedPattern
         ]

    -- TODO:
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

