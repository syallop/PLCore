{-# LANGUAGE
    InstanceSigs
  , GADTs
  , OverloadedStrings
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

data Expected
  = ExpectEither Expected Expected -- Expected either of
  | ExpectOneOf [Text]             -- Expected any of
  | ExpectPredicate Text           -- Failed predicate with label
  | ExpectAnything                 -- Expected anything => got an EOF
  | ExpectN Int Expected           -- Expected a N repetitions
  | ExpectLabel Text Expected      -- Expected something with a label
  deriving Show

instance Document Expected where
  document = text . showExpected

expectNothing :: Expected
expectNothing = ExpectOneOf []

-- Turn an 'Expected' into a bulleted list of each unique expected alternative.
showExpected :: Expected -> Text
showExpected = ("- "<>)
             . Text.intercalate "\n - "
             . List.nub
             . flattenExpected

flattenExpected :: Expected -> [Text]
flattenExpected e = case e of
  ExpectEither es0 es1
    -> flattenExpected es0 ++ flattenExpected es1

  ExpectOneOf ts
    -> ts

  ExpectPredicate t
    -> ["__PREDICATE__" <> t]

  ExpectAnything
    -> ["__ANYTHING__"]

  ExpectN i e
    -> ["__EXACTLY__" <> (Text.pack . show $ i) <> "__{" <> showExpected e <> "}__"]

  ExpectLabel l e
    -> [mconcat ["__LABEL__",l,"__{",showExpected e,"}__"]]

