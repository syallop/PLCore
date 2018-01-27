{-# LANGUAGE
    InstanceSigs
  , GADTs
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TupleSections
  #-}
{-|
Module      : PL.Parser.Expected
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Describe what a Parser expected to see at a position.
-}
-- TODO: Switch text functions to Doc's
module PL.Parser.Expected where

import Prelude hiding (takeWhile,dropWhile,exp)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Text as Text

import PL.Printer.Doc

import PL.PLGrammar.Grammar

data Expected
  = ExpectEither Expected Expected        -- Expected either of
  | ExpectOneOf [Text]                    -- Expected any of
  | ExpectPredicate Text (Maybe Expected) -- Failed predicate with label
  | ExpectAnything                        -- Expected anything => got an EOF
  | ExpectN Int Expected                  -- Expected a N repetitions
  | ExpectLabel Text Expected             -- Expected something with a label
  deriving Show

instance Document Expected where
  document = showExpectedDoc

expectNothing :: Expected
expectNothing = ExpectOneOf []

-- Turn an 'Expected' into a bulleted list of each unique expected alternative.
showExpected :: Expected -> Text
showExpected = Text.intercalate "\n - "
             . List.nub
             . flattenExpected

showExpectedDoc :: Expected -> Doc
showExpectedDoc = foldr (\d0 dAcc -> dAcc <> lineBreak <> DocText " - " <> d0) DocEmpty
                . flattenExpectedDoc

flattenExpectedDoc :: Expected -> [Doc]
flattenExpectedDoc e = case e of
  ExpectEither es0 es1
    -> flattenExpectedDoc es0 <> flattenExpectedDoc es1

  ExpectOneOf ts
    -> let oneOf = map DocText ts
        in if null oneOf
             then [DocText "__EXPECTNOTHING__"]
             else oneOf

  ExpectPredicate label mE
    -> map ((DocText "__PREDICATE__" <> DocText label) <>) $ maybe [] flattenExpectedDoc mE

  ExpectAnything
    -> [DocText "__ANYTHING__"]

  ExpectN i e
    -> [DocText "__EXACTLY__" <> (DocText . Text.pack . show $ i) <> DocText "__{" <> showExpectedDoc e <> DocText "}__"]

  -- Show the label only
  ExpectLabel l e
    -> [DocText l]

-- Turn an 'Expected' into a list of each expected alternative
flattenExpected :: Expected -> [Text]
flattenExpected = map render . flattenExpectedDoc

-- | A Grammar's parser expected to see:
grammarExpects :: forall a. Show a => Grammar a -> Expected
grammarExpects g0 = case g0 of
  -- Expected a single character.
  GAnyChar
    -> ExpectN 1 ExpectAnything

  -- Expected a specific thing.
  GPure a
    -> ExpectOneOf [Text.pack . show $ a]

  -- Expected to fail.
  GEmpty
    -> ExpectOneOf []

  -- Expected one or the other.
  GAlt l r
    -> ExpectEither (grammarExpects l) (grammarExpects r)

  -- Expects something AND a predicate to succeed.
  -- TODO: Capture this desired predicate?
  GIsoMap iso g1
    {--> grammarExpects g1-}
    -> ExpectPredicate "ISOMAP" . Just . grammarExpects $ g1

  -- Expected one thing and then another.
  -- TODO: Express what we wanted after the immediate thing?
  GProductMap g1 g2
    {--> grammarExpects g1-}
    -> ExpectPredicate "THEN" . Just $ (ExpectPredicate "THEN" . Just . grammarExpects $ g1)

