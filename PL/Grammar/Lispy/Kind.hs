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
import PL.Grammar

kindAbs :: Grammar Kind
kindAbs = kind

kind :: Grammar Kind
kind =  kind'
    <|> betweenParens kind'
  where kind' = simpleKind <|> arrowKind

simpleKind :: Grammar Kind
simpleKind = textIs "KIND" *> pure Kind

arrowKind :: Grammar Kind
arrowKind = arrowise <$> (arrow *> kind) <*> kind <*> many kind
  where
    arrowise :: Kind -> Kind -> [Kind] -> Kind
    arrowise from to []     = KindArrow from to
    arrowise from to (t:ts) = KindArrow from (KindArrow to (arrowise to t ts))

