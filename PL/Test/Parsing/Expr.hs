{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
  , GADTs
  #-}
module PL.Test.Parsing.Expr
  ( parsesToSpec
  , parseToSpec
  )
  where

import PL.Commented
import PL.Error
import PL.Expr
import PL.FixPhase

import PL.Test.ExprTestCase

import PLPrinter

import qualified Data.Text as Text
import qualified Data.Map as Map

import Test.Hspec
import PL.Test.Source

-- Test that for each test case, a parser consumes all of some source input in
-- order to produce the intended expression.
parsesToSpec
  :: Map.Map Text.Text ExprTestCase
  -> (Source -> Either Error (ExprFor CommentedPhase, Source))
  -> (ExprFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parsesToSpec testCases parseExpression ppExpr pp
  = describe "All example programs"
  . mapM_ (\(name,testCase)
            -> parseToSpec parseExpression
                           name
                           (_parsesFrom testCase)
                           (_parsesTo testCase)
                           ppExpr
                           pp
          )
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended expression.
parseToSpec
  :: (Source -> Either Error (ExprFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> ExprFor CommentedPhase
  -> (ExprFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parseToSpec parseExpression name inputSource expectedExpr ppExpr pp = it (Text.unpack name) $ case parseExpression inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

  -- No leftovers are allowed and the parsed expression must equal the expected
  -- expression.
  Right (parsedExpr, leftovers)
    | not (Text.null leftovers)
    -> expectationFailure . Text.unpack . render . document . mconcat $
         [ text "Unexpected leftovers:"
         , lineBreak
         , indent1 $ text leftovers
         , lineBreak
         , text "After parsing:"
         , lineBreak
         , indent1 . ppExpr . stripComments $ parsedExpr
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 . ppExpr . stripComments $ expectedExpr
         ]

    -- TODO:
    -- Several hacks are colliding here. Unpick them:
    -- - Stored exprs are used as examples.
    -- - This means they're commented.
    -- - Comments are not currently transfered to the test-cases.
    -- This means we're comparing with the comments stripped. The tradeoff is we
    -- cant test comment parsing.
    | otherwise
     -> let parsedWithoutComments   = stripComments parsedExpr
            expectedWithoutComments = stripComments expectedExpr
         in if parsedWithoutComments == expectedWithoutComments
              then pure ()
              else expectationFailure . Text.unpack . render . document . mconcat $
                     [ text "Successfully parsed without leftovers an unexpected expression. Got:"
                     , lineBreak
                     , indent1 . ppExpr $ parsedWithoutComments
                     , lineBreak
                     , text "But we expected:"
                     , lineBreak
                     , indent1 . ppExpr $ expectedWithoutComments
                     ]

