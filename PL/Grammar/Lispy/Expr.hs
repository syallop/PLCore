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
lamExpr = lambda */ (lamIso \$/ (?abs \* spaceRequired) \*/ exprI)

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

matchArg
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (MatchArg b tb)
matchArg
  = bind
 \|/ matchBinding ?eb
 \|/ matchSum
 \|/ matchProduct
 \|/ matchUnion
 \|/ betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (MatchArg b tb)
matchSum = matchSumI
        \$/ (plus */ natural)
        \*/ (matchArg)
  where
    matchSumI :: Iso (Int,MatchArg b tb) (MatchArg b tb)
    matchSumI = Iso
      (\(ix,m0) -> Just $ MatchSum ix m0)
      (\m0 -> case m0 of
        MatchSum ix m1
          -> Just (ix,m1)

        _ -> Nothing
      )

-- A '*' followed by zero or more matchArgs
matchProduct
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (MatchArg b tb)
matchProduct = matchProductI
            \$/ (star */ (grammarMany $ matchArg))
  where
    matchProductI :: Iso [MatchArg b tb] (MatchArg b tb)
    matchProductI = Iso
      (Just . MatchProduct)
      (\m0 -> case m0 of
        MatchProduct ms
          -> Just ms
        _ -> Nothing
      )


-- A 'U' followed by a type index and a matchArg
matchUnion
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (MatchArg b tb)
matchUnion = matchUnionI
          \$/ (union */ typ ?tb)
          \*/ (matchArg)
  where
    matchUnionI :: Iso (Type tb,MatchArg b tb) (MatchArg b tb)
    matchUnionI = Iso
      (\(t,m) -> Just $ MatchUnion t m)
      (\m0 -> case m0 of
        MatchUnion t0 m1
          -> Just (t0,m1)
        _ -> Nothing
      )

-- A var
matchBinding
  :: (Show b, Show tb)
  => Grammar b
  -> Grammar (MatchArg b tb)
matchBinding eb = matchBindingI \$/ eb
  where
    matchBindingI :: Iso b (MatchArg b tb)
    matchBindingI = Iso
      (Just . MatchBinding)
      (\m0 -> case m0 of
        MatchBinding b
          -> Just b
        _ -> Nothing
      )

-- A '?'
bind
  :: (Show b
     ,Show tb
     ,Eq b
     ,Eq tb
     )
  => Grammar (MatchArg b tb)
bind = question */ GPure Bind

-- "CASE", then an expr then casebranches
caseExpr :: (Show b,Show abs,Show tb,Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
caseExpr = caseAnalysisI
        \$/ (textIs "CASE" */ exprI)
        \*/ (caseBody)
  where
    caseAnalysisI :: Iso (Expr b abs tb,(CaseBranches (Expr b abs tb) (MatchArg b tb))) (Expr b abs tb)
    caseAnalysisI = Iso
      (\(e0,cb) -> Just $ CaseAnalysis $ Case e0 cb)
      (\e0 -> case e0 of
        CaseAnalysis (Case e1 bs)
          -> Just (e1,bs)
        _ -> Nothing
      )

-- Either someCaseBranches or
caseBody
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBody = caseBranches
        \|/ defaultOnly

-- One or many casebranch then a possible default expr
caseBranches
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBranches = caseBranchesI
            \$/ someCaseBranches
            \*/ ((justI \$/ exprI) \|/ GPure Nothing)
  where
    caseBranchesI :: Iso (NonEmpty (CaseBranch (Expr b abs tb)(MatchArg b tb))
                         ,Maybe (Expr b abs tb))

                         (CaseBranches (Expr b abs tb) (MatchArg b tb))
    caseBranchesI = Iso
      (\(bs,mDef) -> Just $ CaseBranches bs mDef)
      (\cb -> case cb of
        CaseBranches bs mDef
          -> Just (bs,mDef)
        _ -> Nothing
      )

    justI :: Iso a (Maybe a)
    justI = Iso
      (\a -> Just $ Just a)
      id

-- A non-empty list of caseBranch
someCaseBranches
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)))
someCaseBranches = nonEmptyI
                \$/ caseBranch
                \*/ (grammarMany $ caseBranch)
  where
    nonEmptyI :: Iso (a,[a]) (NonEmpty a)
    nonEmptyI = Iso
      (\(a,as) -> Just $ a :| as)
      (\ne -> let (a,mNE) = NE.uncons ne
                 in Just (a,maybe [] NE.toList mNE)
      )

-- A single case branch is a matchArg pattern, then a result expression
caseBranch
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (CaseBranch (Expr b abs tb) (MatchArg b tb))
caseBranch = caseBranch'
          \|/ betweenParens caseBranch'
  where
    caseBranch' = caseBranchI
               \$/ (charIs '|' */ matchArg)
               \*/ (exprI)

    caseBranchI :: Iso (MatchArg b tb,Expr b abs tb) (CaseBranch (Expr b abs tb) (MatchArg b tb))
    caseBranchI = Iso
      (\(m,e) -> Just $ CaseBranch m e)
      (\(CaseBranch m e) -> Just (m,e))

-- A default case branch only
defaultOnly
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
defaultOnly = defaultOnlyI \$/ exprI
  where
    defaultOnlyI :: Iso (Expr b abs tb) (CaseBranches (Expr b abs tb) (MatchArg b tb))
    defaultOnlyI = Iso
      (Just . DefaultOnly)
      (\cb -> case cb of
        DefaultOnly e
          -> Just e
        _ -> Nothing
      )

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
  , caseExpr
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

