{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
{-|
Module      : ExprSpec.Boolean
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

HSpec tests for PL.Parser.
-}
module ParserSpec where

import PL.Parser
import PL.Printer
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as Text
import Control.Applicative

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Coerce

-- A string of text with no spaces
newtype TokenText = TokenText{_unTokenText :: Text} deriving Show
instance Arbitrary TokenText where
  arbitrary = TokenText <$> spaceLessText

instance Arbitrary (Parser ()) where
  arbitrary = oneof [arbitraryChar
                    ,arbitraryText
                    ,arbitraryThen
                    ,arbitraryAlt
                    ]

arbitraryChar :: Gen (Parser ())
arbitraryChar = charIs <$> arbitrary

arbitraryText :: Gen (Parser ())
arbitraryText = textIs <$> arbitrary

arbitraryThen :: Gen (Parser ())
arbitraryThen = (<>) <$> arbitrary <*> arbitrary

arbitraryAlt :: Gen (Parser ())
arbitraryAlt = (<|>) <$> arbitrary <*> arbitrary




spaceLessText :: Gen Text
spaceLessText = arbitrary `suchThat` (not . Text.any (`elem` [' ','\n']))

spec :: Spec
spec = describe "Parser" $ sequence_ [charSpec,textSpec]

-- Test the character parsers
charSpec :: Spec
charSpec = describe "Single characters" $ do
  prop "parse as singletons" prop_charParse
  prop "parse with trailing characters" prop_charTrailingParse

-- A character parses as a singleton string
prop_charParse :: Char -> Bool
prop_charParse c =
  charIs c `parses` (Text.singleton c)

-- A character parses with any amount of trailing characters of any kind
-- AND The remaining characters left in the cursor have their leading spaces and newlines dropped.
prop_charTrailingParse :: Char -> Text -> Bool
prop_charTrailingParse c trailing =
  charIs c `parsesSuchThat` (Text.cons c trailing)
           $ \() csr
              -> remainder csr == (Text.dropWhile (`elem` [' ','\n']) trailing)

-- Test the text parsers
textSpec :: Spec
textSpec = describe "Text" $ do
  prop "exact matches"         prop_textParse
  prop "with trailing text"    prop_textTrailingParse
  prop "appended"              prop_textAppendParse
  prop "alternative"           prop_textAltParse
  prop "backtrack alternative" prop_textAltBacktracksParse


-- 'textIs txt' parses 'txt'
prop_textParse :: Text -> Bool
prop_textParse txt =
  textIs txt `parses` txt

-- 'textIs txt' parses 'txt' appended to a space and some arbitrary trailing txt.
-- The remaining txt has leading spaces and newlines dropped.
prop_textTrailingParse :: Text -> Text -> Bool
prop_textTrailingParse txt trailing =
  (textIs txt) `parsesSuchThat` (txt <> " " <> trailing)
               $ \() csr
                  -> remainder csr == (Text.dropWhile (`elem` [' ','\n']) trailing)

-- Text appended between a space is parsed by appending textIs parsers
-- (textIs txt0 <> textIs txt1) `parses` (txt0 <> " " <> txt1)
prop_textAppendParse :: TokenText -> TokenText -> Bool
prop_textAppendParse txt0 txt1 =
  (textIs (coerce txt0) <> textIs (coerce txt1)) `parses` (coerce txt0 <> " " <> coerce txt1)

-- An alternative of two 'textIs's succeeds on both text
prop_textAltParse :: TokenText -> TokenText -> Bool
prop_textAltParse txt0 txt1 = and
  [(textIs (coerce txt0) <|> textIs (coerce txt1)) `parses` (coerce txt0)
  ,(textIs (coerce txt0) <|> textIs (coerce txt1)) `parses` (coerce txt1)
  ]


-- An alternative backtracks by default
prop_textAltBacktracksParse :: TokenText -> TokenText -> Bool
prop_textAltBacktracksParse txt trailing =
  (textIs ((coerce txt) <> (coerce trailing)) <|> textIs (coerce txt)) `parses` (coerce txt)


-- True if the text is parsed successfully to some result, perhaps with some leftovers
parses :: Parser a -> Text -> Bool
parses p txt = parsesSuchThat p txt (\_ _ -> True)

-- True if the text parses successfully to some result and then a predicate succeeds
parsesSuchThat :: Parser a -> Text -> (a -> Cursor -> Bool) -> Bool
parsesSuchThat p txt pred = case runParser p txt of
  ParseFailure _ _
    -> False
  ParseSuccess a c
    -> pred a c

