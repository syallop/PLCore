{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

import PL.Grammar
import PL.Grammar.Lispy.Kind
import PL.Grammar.Lispy.Type

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

typeAbs :: Ord tb => Grammar tb -> Grammar (Type tb)
typeAbs tb = typ tb

-- A lambda followed by one or more type abstractions then an expression.
lamExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
lamExpr = lamise <$> (lambda *> ?abs) <*> many ?abs <*> exprI

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- A big lambda followed by one or more kind abstractions then an expression
bigLamExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
bigLamExpr = bigLamise <$> (bigLambda *> kind) <*> many kind  <*> exprI

-- Chain big lambda
bigLamise :: Kind -> [Kind] -> Expr b abs tb -> Expr b abs tb
bigLamise a0 []     e = BigLam a0 e
bigLamise a0 (a:as) e = BigLam a0 $ bigLamise a as e


-- An '@' followed by two or more expressions
appExpr :: (Ord tb, Implicits b abs tb) => Grammar (Expr b abs tb)
appExpr = appise <$> (at *> exprI) <*> exprI <*> many exprI

-- Chain application
appise :: Expr b abs tb -> Expr b abs tb -> [Expr b abs tb] -> Expr b abs tb
appise f x []     = App f x
appise f x (y:ys) = appise (App f x) y ys


-- A "@@" followed by two or more expressions
bigAppExpr :: (Ord tb, Implicits b abs tb) => Grammar (Expr b abs tb)
bigAppExpr = bigAppise <$> (at *> exprI) <*> (typ ?tb) <*> many (typ ?tb)

-- Chain big application
bigAppise :: Expr b abs tb -> Type tb -> [Type tb] -> Expr b abs tb
bigAppise f x []     = BigApp f x
bigAppise f x (y:ys) = bigAppise (BigApp f x) y ys


bindingExpr :: Grammar b -> Grammar (Expr b abs tb)
bindingExpr eb = Binding <$> eb

-- A var used as a binding
varBindingExpr :: Grammar (Expr Var abs tb)
varBindingExpr = Binding <$> var

var :: Grammar Var
var = mkVar <$> natural


-- A '+' followed by an index, an expression and two or more types
sumExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
sumExpr = sumise <$> (plus *> natural) <*> exprI <*> typ ?tb <*> typ ?tb <*> many (typ ?tb)

sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
productExpr = productise <$> (star *> many exprI)

productise :: [Expr b abs tb] -> Expr b abs tb
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
unionExpr = unionise <$> (union *> (typ ?tb)) <*> exprI <*> typ ?tb <*> typ ?tb <*> many (typ ?tb)

unionise :: (Ord tb, Implicits b abs tb) => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: (Ord tb,Implicits b abs tb) => Grammar (MatchArg b tb)
matchArg
  = bind
 <|> matchBinding ?eb
 <|> matchSum
 <|> matchProduct
 <|> matchUnion
 <|> betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum :: (Ord tb, Implicits b abs tb) => Grammar (MatchArg b tb)
matchSum = MatchSum <$> (plus *> natural) <*> matchArg

-- A '*' followed by zero or more matchArgs
matchProduct :: (Ord tb, Implicits b abs tb) => Grammar (MatchArg b tb)
matchProduct = MatchProduct <$> (star *> many matchArg)

-- A 'U' followed by a type index and a matchArg
matchUnion :: (Ord tb,Implicits b abs tb) => Grammar (MatchArg b tb)
matchUnion = MatchUnion <$> (union *> (typ ?tb)) <*> matchArg

-- A var
matchBinding :: Grammar b -> Grammar (MatchArg b tb)
matchBinding eb = MatchBinding <$> eb

-- A '?'
bind :: Grammar (MatchArg b tb)
bind = question *> pure Bind

-- "CASE", then an expr then casebranches
caseExpr :: (Ord tb,Implicits b abs tb) => Grammar (Expr b abs tb)
caseExpr = CaseAnalysis <$> (Case <$> (textIs "CASE" *> exprI) <*> caseBody)

-- Either someCaseBranches or
caseBody :: (Ord tb,Implicits b abs tb) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBody = caseBranches <|> defaultOnly

-- One or many casebranch then a possible default expr
caseBranches :: (Ord tb,Implicits b abs tb) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBranches = CaseBranches <$> someCaseBranches <*> ((Just <$> exprI) <|> pure Nothing)

-- A non-empty list of caseBranch
someCaseBranches :: (Ord tb,Implicits b abs tb) => Grammar (NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)))
someCaseBranches = (:|) <$> caseBranch <*> many caseBranch

-- A single case branch is a matchArg pattern, then a result expression
caseBranch :: (Ord tb,Implicits b abs tb) => Grammar (CaseBranch (Expr b abs tb) (MatchArg b tb))
caseBranch = caseBranch' <|> betweenParens caseBranch'
  where
    caseBranch' = CaseBranch <$> (charIs '|' *> matchArg) <*> exprI

-- A default case branch only
defaultOnly :: (Ord tb,Implicits b abs tb) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
defaultOnly = DefaultOnly <$> exprI


-- Implicitly bind Grammars for expression bindings, abstractions and type bindings
type Implicits b abs tb = (?eb :: Grammar b,?abs :: Grammar abs,?tb :: Grammar tb)

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI :: (Ord tb, Implicits b abs tb) => Grammar (Expr b abs tb)
exprI = alternatives
  [lamExpr
  ,bigLamExpr
  ,appExpr
  ,bigAppExpr
  ,sumExpr
  ,productExpr
  ,unionExpr
  ,caseExpr
  ,bindingExpr ?eb
  ,betweenParens exprI
  ]

-- Parse an expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. Type)
-- - Type bindings          (E.G. Var)
expr :: Ord tb => Grammar b -> Grammar abs -> Grammar tb -> Grammar (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI
