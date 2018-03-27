{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PL.Grammar.Lispy.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr with a lisp-like syntax.
-}
module PL.Grammar.Lispy.Expr where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import PLGrammar
import PLGrammar.Iso

import PL.Grammar.Lispy.ExprIso
import PL.Grammar.Lispy.Case
import PL.Grammar.Lispy.Kind
import PL.Grammar.Lispy.Type

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

typeAbs
  :: (Show tb
     ,Ord tb
     )
  => Grammar tb
  -> Grammar (Type tb)
typeAbs tb = typ tb

-- Implicitly bind Grammars for expression bindings, abstractions and type bindings
type Implicits b abs tb = (?eb :: Grammar b,?abs :: Grammar abs,?tb :: Grammar tb)

-- Bind the given grammars into a Grammar which takes them implicitly
using
  :: Grammar b
  -> Grammar abs
  -> Grammar tb
  -> (Implicits b abs tb => Grammar a)
  -> Grammar a
using b abs tb a =
  let ?eb = b
      ?abs = abs
      ?tb = tb
    in a

implicitly
  :: (Grammar b -> Grammar abs -> Grammar tb -> Grammar a)
  -> (Implicits b abs tb => Grammar a)
implicitly f = f ?eb ?abs ?tb


type Constraints b abs tb =
  (Show b
  ,Show abs
  ,Show tb
  ,Ord tb
  ,Implicits b abs tb
  ,Eq b
  ,Eq abs
  )

type ExprGrammar b abs tb = Constraints b abs tb => Grammar (Expr b abs tb)

-- A lambda followed by one or more type abstractions then an expression.
lamExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
lamExpr = lambda */ (lamIso \$/ ?abs \*/ (spaceRequired */ exprI))

-- A big lambda followed by one or more kind abstractions then an expression
bigLamExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
bigLamExpr = bigLambda */ (bigLamIso \$/ (kind \* spaceRequired) \*/ exprI)

-- An at followed by two or more expressions
appExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
appExpr = at */ (appIso \$/ (exprI \* spaceRequired) \*/ exprI)

-- A big at followed by two or more expressions
bigAppExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
bigAppExpr = bigAt */ (bigAppIso \$/ (exprI \* spaceRequired) \*/ typ ?tb)

bindingExpr
  :: (Show b,Show tb,Show abs)
  => Grammar b
  -> Grammar (Expr b abs tb)
bindingExpr eb = bindingIso \$/ eb

var
  :: Grammar Var
var = varIso \$/ natural

-- A plus followed by an index, an expression and two or more types
sumExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
sumExpr = plus */ (sumIso \$/ (natural \* spaceRequired)
                          \*/ exprI
                          \*/ (grammarMany (spaceRequired */ typ ?tb)))

-- A star followed by zero or more expressions
productExpr :: Constraints b abs tb => Grammar (Expr b abs tb)
productExpr = star */ (productIso \$/ grammarMany (spaceRequired */ exprI))

-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: forall b abs tb. Constraints b abs tb => Grammar (Expr b abs tb)
unionExpr = union */ (unionIso \$/ (typ ?tb \* spaceRequired)
                               \*/ exprI
                               \*/ (setIso \$/ grammarMany (spaceRequired */ typ ?tb)))


-- "CASE", then an expr then casebranches
--
-- CASE(\Scrut 0)
--  (((|? (\MatchedFoo 0))
--    (|? (\MatchedBar 0)))
--        (\Default 0))
--
-- or
--
-- CASE
--  (\Scrut 0)
--  (((|? (\MatchedFoo 0))
--    (|? (\MatchedBar 0)))
--        (\Default 0))
caseAnalysis :: (Show b,Show abs,Show tb,Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
caseAnalysis = textIs "CASE" */ spaceAllowed */ (caseAnalysisIso \$/ caseStatement exprI)

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI :: Constraints b abs tb => Grammar (Expr b abs tb)
exprI = alternatives
  [ lamExpr
  , bigLamExpr
  , appExpr
  , bigAppExpr
  , sumExpr
  , productExpr
  , unionExpr
  , bindingExpr ?eb
  , caseAnalysis
  , try $ betweenParens exprI
  ]

-- Parse an expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. Type)
-- - Type bindings          (E.G. Var)
expr
  :: (Show b, Show abs, Show tb, Ord tb, Eq b, Eq abs)
  => Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Grammar (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI

