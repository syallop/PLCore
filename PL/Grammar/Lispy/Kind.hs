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

import PL.PLGrammar.Grammar
import PL.PLGrammar.Iso

kindAbs :: Grammar Kind
kindAbs = kind

kind :: Grammar Kind
kind =  kind'
    \|/ betweenParens kind'
  where kind' = simpleKind \|/ arrowKind

simpleKind :: Grammar Kind
simpleKind = textIs "KIND" */ GPure Kind

arrowKind :: Grammar Kind
arrowKind = arrowiseI
         \$/ (arrow */ kind)
         \*/ (kind)
         \*/ (grammarMany $ kind)
  where
    -- Iso between a Kind and its 'arrowised' form where we have a from, a to
    -- and zero or many trailing kinds
    arrowiseI :: Iso (Kind,(Kind,[Kind])) Kind
    arrowiseI = Iso
      (\(from,(to,ts)) -> case ts of
          []     -> Just $ KindArrow from to
          (t:ts) -> case parseIso arrowiseI (to,(t,ts)) of
                      Nothing
                        -> Nothing

                      Just k
                        -> Just $ KindArrow from $ KindArrow to $ k
      )
      (\k -> case k of
               Kind
                 -> Nothing

               KindArrow k ks
                 -> case printIso arrowiseI ks of
                      Just (k0,(k1,k2s))
                        -> Just (k,(k0,k1:k2s))
                      Nothing
                        -> Just (k,(Kind,[]))
      )

