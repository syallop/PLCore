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
import PL.Expr
import PL.Error
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Name
import PL.Type.Eq
import PL.TypeCtx
import PL.Var
import PL.FixPhase
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
  -> (Source -> Either Error (TypeFor CommentedPhase, Source))
  -> (TypeFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parsesToTypesSpec testCases parseType ppType pp
  = describe "All example types"
  . mapM_ (\(name,testCase)
            -> parseToTypeSpec parseType
                               name
                               (_parsesFrom testCase)
                               (_parsesTo testCase)
                               ppType
                               pp
          )
  . Map.toList
  $ testCases

-- | Test that a parser consumes all of some source input in order to produce
-- the intended type.
parseToTypeSpec
  :: (Source -> Either Error (TypeFor CommentedPhase,Source))
  -> Text.Text
  -> Source
  -> TypeFor CommentedPhase
  -> (TypeFor CommentedPhase -> Doc)
  -> PPError DefaultPhase
  -> Spec
parseToTypeSpec parseType name inputSource expectedType ppType pp = it (Text.unpack name) $ case parseType inputSource of
  Left err
    -> expectationFailure . Text.unpack . render . ppError pp $ err

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
         , indent1 . ppType $ parsedType
         , lineBreak
         , text "when we expected: "
         , lineBreak
         , indent1 . ppType $ expectedType
         ]

    -- TODO:
    -- Several hacks are colliding here. Unpick them:
    -- - Stored types are used as examples.
    -- - This means they're commented.
    -- - Comments are not currently transfered to the test-cases.
    -- This means we're comparing with the comments stripped. The tradeoff is we
    -- cant test comment parsing.
    | otherwise
     -> let parsedWithoutComments   = stripTypeComments parsedType
            expectedWithoutComments = stripTypeComments expectedType
         in if parsedWithoutComments == expectedWithoutComments
              then pure ()
              else expectationFailure . Text.unpack . render . document . mconcat $
                     [ text "Successfully parsed without leftovers an unexpected type. Got:"
                     , lineBreak
                     , indent1 . ppType $ parsedWithoutComments
                     , lineBreak
                     , text "But we expected:"
                     , lineBreak
                     , indent1 . ppType $ expectedWithoutComments
                     ]

    | otherwise
     -> pure ()

