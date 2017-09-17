{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PL.Parser.Lispy.Kind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Parser for PL.Kind which consumes a lisp-like syntax.
-}
module PL.Parser.Lispy.Kind where

import Control.Applicative

import PL.Kind
import PL.Parser

kindAbs :: Parser Kind
kindAbs = kind

kind :: Parser Kind
kind =  kind'
    <|> betweenParens kind'
  where kind' = simpleKind <|> arrowKind

simpleKind :: Parser Kind
simpleKind = textIs "KIND" *> pure Kind

arrowKind :: Parser Kind
arrowKind = arrowise <$> (arrow *> kind) <*> kind <*> many kind
  where
    arrowise :: Kind -> Kind -> [Kind] -> Kind
    arrowise from to []     = KindArrow from to
    arrowise from to (t:ts) = KindArrow from (KindArrow to (arrowise to t ts))

