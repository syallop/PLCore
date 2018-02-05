{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Grammar.Lispy.Kind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Kind with a lisp-like syntax.
-}
module PL.Grammar.Lispy.Kind where

import Control.Applicative

import PL.Kind

import PL.Grammar.Lispy.KindIso

import PLGrammar
import PLGrammar.Iso

kindAbs :: Grammar Kind
kindAbs = kind

kind :: Grammar Kind
kind =  kind'
    \|/ (try $ betweenParens kind')
  where
    kind' :: Grammar Kind
    kind' = simpleKind \|/ arrowKind

simpleKind :: Grammar Kind
simpleKind = textIs "KIND" */ GPure Kind

arrowKind :: Grammar Kind
arrowKind = arrow */ (kindArrowIso \$/ kind \*/ (spaceRequired */ kind))

