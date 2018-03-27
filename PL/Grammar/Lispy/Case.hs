{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PL.Grammar.Lispy.Case
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr.Case with a lisp-like syntax.
-}
module PL.Grammar.Lispy.Case where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.List.NonEmpty

import PLGrammar
import PLGrammar.Iso

import PL.Grammar.Lispy.MatchArg
import PL.Grammar.Lispy.CaseIso
import PL.Grammar.Lispy.Kind
import PL.Grammar.Lispy.Type

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

-- An entire case statement starts with "CASE" followed by a scrutinee then the
-- case branches.
-- (\Scrut 0)
-- ((|? (\Matchedfoo 0))
-- (|? (\Matchedbar 0)))
--     (\Default 0)
-- or
-- (\Scrut 0)
--   (((|? (\Matchedfoo 0))
--     (|? (\Matchedbar 0)))
--         (\Default 0))
caseStatement
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (Case (Expr b abs tb) (MatchArg b tb))
caseStatement exprGr
  = caseIso \$/ exprGr
            \*/ (spaceRequired */ caseBody' exprGr)
  where
    caseBody' exprGr
      = (try . betweenParens . caseBody $ exprGr)
     \|/ caseBody exprGr

-- Either someCaseBranches or
-- ((|? (\Far 0))
--  (|? (\Bar 0)))
--      (\Baz 0)
-- or
-- \Foo 0
caseBody
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBody scrutineeGr =  (try $ caseBranches scrutineeGr)
                    \|/ (defaultOnly scrutineeGr)

-- One or many casebranch then a possible default expr
-- ((|? (\Far 0))
--  (|? (\Bar 0)))
--      (\Baz 0)
caseBranches
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBranches scrutineeGr =
  caseBranchesIso
    \$/ (someCaseBranches' scrutineeGr)
    \*/ (try (justI \$/ spaceAllowed */ scrutineeGr) \|/ GPure Nothing)
  where
    someCaseBranches' scrutineeGr =
      (try . betweenParens . someCaseBranches $ scrutineeGr)
        \|/ (someCaseBranches scrutineeGr)

    justI :: Iso a (Maybe a)
    justI = Iso
      ["just"]
      (\a -> Just $ Just a)
      id

-- A non-empty list of caseBranch
-- (|? (\Foo 0)) (|? (\Bar 0))
someCaseBranches
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
  )
  => Grammar (Expr b abs tb)
  -> Grammar (NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)))
someCaseBranches exprI =
  nonEmptyI
    \$/ (caseBranch exprI)
    \*/ (grammarMany $ spaceRequired */ caseBranch exprI)
  where
    nonEmptyI :: Iso (a,[a]) (NonEmpty a)
    nonEmptyI = Iso
      ["nonEmpty"]
      (\(a,as) -> Just $ a :| as)
      (\ne -> let (a,mNE) = NE.uncons ne
                 in Just (a,maybe [] NE.toList mNE)
      )

-- A single case branch is a matchArg pattern, then a result expression
-- E.G.: |? (\Foo 0)
caseBranch
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranch (Expr b abs tb) (MatchArg b tb))
caseBranch exprI = (try caseBranch')
          \|/ (try $ betweenParens caseBranch')
  where
    caseBranch' = charIs '|' */ (caseBranchIso \$/ matchArg
                                               \*/ (spaceRequired */ exprI))

-- A default case branch only
defaultOnly
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
defaultOnly exprI = defaultOnlyIso \$/ exprI

