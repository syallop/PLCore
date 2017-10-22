{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
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

import PL.Grammar
import PL.Grammar.Lispy.Kind
import PL.Grammar.Lispy.Type

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

import PL.Iso

typeAbs :: Ord tb => Grammar tb -> Grammar (Type tb)
typeAbs tb = typ tb

-- A lambda followed by one or more type abstractions then an expression.
lamExpr :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
lamExpr = lamiseI \$/ (lambda */ ?abs) \*/ (grammarMany ?abs) \*/ exprI
  where
    lamiseI :: Iso (abs,([abs],Expr b abs tb)) (Expr b abs tb)
    lamiseI = Iso
      (\(a0,(as,e)) -> Just $ lamise a0 as e)

      (\e -> case e of
        Lam a0 e0
          -> case printIso lamiseI e0 of
               Just (a1,(a2s,e1))
                 -> Just (a0,(a1:a2s,e1))

               Nothing
                 -> Just (a0,([],e0))

        _ -> Nothing
      )

    -- Chain lambda
    lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
    lamise a0 []     e = Lam a0 e
    lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- A big lambda followed by one or more kind abstractions then an expression
bigLamExpr :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
bigLamExpr = bigLamiseI \$/ (bigLambda */ kind) \*/ grammarMany kind  \*/ exprI
  where
    bigLamiseI :: Iso (Kind,([Kind],Expr b abs tb)) (Expr b abs tb)
    bigLamiseI = Iso
      (\(k0,(k1s,e)) -> Just $ bigLamise k0 k1s e)

      (\e -> case e of
        BigLam k0 e0
          -> case printIso bigLamiseI e0 of
               Just (k1,(k2s,e1))
                 -> Just (k0,(k1:k2s,e1))

               Nothing
                 -> Just (k0,([],e0))

        _ -> Nothing
      )

    -- Chain big lambda
    bigLamise :: Kind -> [Kind] -> Expr b abs tb -> Expr b abs tb
    bigLamise a0 []     e = BigLam a0 e
    bigLamise a0 (a:as) e = BigLam a0 $ bigLamise a as e


-- An '@' followed by two or more expressions
appExpr :: (Ord tb, Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
appExpr = appiseI \$/ (at */ exprI) \*/ exprI \*/ grammarMany exprI
  where
    appiseI :: Iso (Expr b abs tb,(Expr b abs tb,[Expr b abs tb])) (Expr b abs tb)
    appiseI = Iso
      (\(e0,(e1,e2s)) -> Just $ appise e0 e1 e2s)

      (\e0 -> case e0 of
        App e1 e2
          -> case printIso appiseI e2 of
               Just (e2,(e3,e4s))
                 -> Just (e1,(e2,e3:e4s))

               Nothing
                 -> Just (e1,(e2,[]))

        _ -> Nothing
      )

    -- Chain application
    appise :: Expr b abs tb -> Expr b abs tb -> [Expr b abs tb] -> Expr b abs tb
    appise f x []     = App f x
    appise f x (y:ys) = appise (App f x) y ys


-- A "@@" followed by two or more expressions
bigAppExpr :: (Ord tb, Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
bigAppExpr = bigAppiseI \$/ (at */ exprI) \*/ (typ ?tb) \*/ grammarMany (typ ?tb)
  where
    bigAppiseI :: Iso (Expr b abs tb,(Type tb,[Type tb])) (Expr b abs tb)
    bigAppiseI = Iso
      (\(e0,(t0,t1s)) -> Just $ bigAppise e0 t0 t1s)

      (\e0 -> case e0 of
        BigApp e1 t0
          -> case printIso bigAppiseI e1 of
               Just (e2,(t1,t2s))
                 -> Just (e1,(t0,(t1:t2s)))

               Nothing
                 -> Just (e1,(t0,[]))
        _ -> Nothing
      )

    -- Chain big application
    bigAppise :: Expr b abs tb -> Type tb -> [Type tb] -> Expr b abs tb
    bigAppise f x []     = BigApp f x
    bigAppise f x (y:ys) = bigAppise (BigApp f x) y ys


bindingExpr :: Grammar b -> Grammar (Expr b abs tb)
bindingExpr eb = bindingI \$/ eb
  where
    bindingI :: Iso b (Expr b abs tb)
    bindingI = Iso
      (Just . Binding)
      (\e -> case e of
        Binding b
          -> Just b
        _ -> Nothing
      )

var :: Grammar Var
var = varI \$/ natural
  where
    varI :: Iso Int Var
    varI = Iso
      (Just . mkVar)
      (Just . fromEnum) -- TODO: Partial

-- A '+' followed by an index, an expression and two or more types
sumExpr :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
sumExpr = sumiseI \$/ (plus */ natural) \*/ exprI \*/ typ ?tb \*/ typ ?tb \*/ grammarMany (typ ?tb)
  where
    sumiseI :: Iso (Int,(Expr b abs tb,(Type tb,(Type tb,[Type tb])))) (Expr b abs tb)
    sumiseI = Iso
      (\(ix,(e0,(t0,(t1,t2s)))) -> Just $ sumise ix e0 t0 t1 t2s)

      (\e0 -> case e0 of
        Sum e1 ix ts
          -> case ts of
               []         -> Nothing
               (_:[])     -> Nothing
               (t0:t1:ts) -> Just (ix,(e0,(t0,(t1,ts))))

        _ -> Nothing
      )

    sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
    sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
productExpr = productiseI \$/ (star */ grammarMany exprI)
  where
    productiseI :: Iso [Expr b abs tb] (Expr b abs tb)
    productiseI = Iso
      (Just . Product)
      (\e -> case e of
        Product es
          -> Just es
        _ -> Nothing
      )


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: forall b abs tb. (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
unionExpr = unioniseI \$/ (union */ (typ ?tb)) \*/ exprI \*/ typ ?tb \*/ typ ?tb \*/ grammarMany (typ ?tb)
  where
    unioniseI :: Iso (Type tb,(Expr b abs tb,(Type tb,(Type tb,[Type tb])))) (Expr b abs tb)
    unioniseI = Iso
      (\(tIx0,(e0,(t0,(t1,t2s)))) -> Just $ unionise tIx0 e0 t0 t1 t2s)
      (\e0 -> case e0 of
        Union e1 tIx ts
          -> case Set.toList ts of
               []         -> Nothing
               (_:[])     -> Nothing
               (t0:t1:t2s) -> Just (tIx,(e1,(t0,(t1,t2s))))

        _ -> Nothing
      )

    unionise :: (Ord tb, Implicits b abs tb) => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
    unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (MatchArg b tb)
matchArg
  = bind
 \|/ matchBinding ?eb
 \|/ matchSum
 \|/ matchProduct
 \|/ matchUnion
 \|/ betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum :: (Ord tb, Implicits b abs tb,Eq b,Eq abs) => Grammar (MatchArg b tb)
matchSum = matchSumI \$/ (plus */ natural) \*/ matchArg
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
matchProduct :: (Ord tb, Implicits b abs tb,Eq b,Eq abs) => Grammar (MatchArg b tb)
matchProduct = matchProductI \$/ (star */ grammarMany matchArg)
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
matchUnion :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (MatchArg b tb)
matchUnion = matchUnionI \$/ (union */ (typ ?tb)) \*/ matchArg
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
matchBinding :: Grammar b -> Grammar (MatchArg b tb)
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
bind :: (Eq b,Eq tb) => Grammar (MatchArg b tb)
bind = question */ GPure Bind

-- "CASE", then an expr then casebranches
caseExpr :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
caseExpr = caseAnalysisI \$/ (textIs "CASE" */ exprI) \*/ caseBody
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
caseBody :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBody = caseBranches \|/ defaultOnly

-- One or many casebranch then a possible default expr
caseBranches :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBranches = caseBranchesI \$/ someCaseBranches \*/ ((justI \$/ exprI) \|/ GPure Nothing)
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
someCaseBranches :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)))
someCaseBranches = nonEmptyI \$/ caseBranch \*/ grammarMany caseBranch
  where
    nonEmptyI :: Iso (a,[a]) (NonEmpty a)
    nonEmptyI = Iso
      (\(a,as) -> Just $ a :| as)
      (\ne -> let (a,mNE) = NE.uncons ne
                 in Just (a,maybe [] NE.toList mNE)
      )

-- A single case branch is a matchArg pattern, then a result expression
caseBranch :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (CaseBranch (Expr b abs tb) (MatchArg b tb))
caseBranch = caseBranch' \|/ betweenParens caseBranch'
  where
    caseBranch' = caseBranchI \$/ (charIs '|' */ matchArg) \*/ exprI

    caseBranchI :: Iso (MatchArg b tb,Expr b abs tb) (CaseBranch (Expr b abs tb) (MatchArg b tb))
    caseBranchI = Iso
      (\(m,e) -> Just $ CaseBranch m e)
      (\(CaseBranch m e) -> Just (m,e))

-- A default case branch only
defaultOnly :: (Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
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

-- Implicitly bind Grammars for expression bindings, abstractions and type bindings
type Implicits b abs tb = (?eb :: Grammar b,?abs :: Grammar abs,?tb :: Grammar tb)

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI :: (Ord tb, Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
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
expr :: (Ord tb,Eq b,Eq abs) => Grammar b -> Grammar abs -> Grammar tb -> Grammar (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI
