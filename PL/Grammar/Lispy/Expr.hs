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

import PL.PLGrammar.Grammar
import PL.PLGrammar.Iso

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

-- A lambda followed by one or more type abstractions then an expression.
lamExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
lamExpr = tokenThenMany1ThenSomething lambda ?abs exprI lamiseI
  where
    lamiseI :: Iso ([abs], Expr b abs tb) (Expr b abs tb)
    lamiseI = Iso
      (\(as,e0) -> Just $ lamise as e0)
      (\e0 -> case e0 of
        Lam a0 e1
          -> case printIso lamiseI e1 of
               -- More lambdas and a final expression can be cons'd
               Just (a1s, e2)
                 -> Just (a0:a1s, e2)

               -- No further lambdas. Just this abstraction and the expression.
               Nothing
                 -> Just ([a0], e1)
        _
          -> Nothing
      )


    lamise :: [abs] -> Expr b abs tb -> Expr b abs tb
    lamise []     _ = error "Lamise should only be called with non-empty lists"
    lamise [a]    e = Lam a e
    lamise (a:as) e = Lam a $ lamise as e

-- A big lambda followed by one or more kind abstractions then an expression
bigLamExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
bigLamExpr = tokenThenMany1ThenSomething bigLambda kind exprI bigLamiseI
  where
    bigLamiseI :: Iso ([Kind],Expr b abs tb) (Expr b abs tb)
    bigLamiseI = Iso
      (\(ks,e0) -> Just $ bigLamise ks e0)
      (\e0 -> case e0 of
        BigLam k0 e1
          -> case printIso bigLamiseI e1 of
               -- More big lambdas and a final expression can be cons'd
               Just (ks1, e2)
                 -> Just (k0:ks1, e2)

               -- No further big lambdas. Just this abstraction and the
               -- expression.
               Nothing
                 -> Just ([k0], e1)

        _ -> Nothing
      )

    bigLamise :: [Kind] -> Expr b abs tb -> Expr b abs tb
    bigLamise []     _ = error "bigLamise should only be called with non-empty lists"
    bigLamise [k]    e = BigLam k e
    bigLamise (k:ks) e = BigLam k $ bigLamise ks e


-- An '@' followed by two or more expressions
appExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
appExpr = appiseI
       \$/ (at */ exprI)
       \*/ (exprI)
       \*/ (grammarMany $ exprI)
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
bigAppExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
bigAppExpr = bigAppiseI
          \$/ (at */ exprI)
          \*/ (typ ?tb)
          \*/ (grammarMany $ typ ?tb)
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


bindingExpr
  :: (Show b,Show tb,Show abs)
  => Grammar b
  -> Grammar (Expr b abs tb)
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

var
  :: Grammar Var
var = varI \$/ natural
  where
    varI :: Iso Int Var
    varI = Iso
      (Just . mkVar)
      (Just . fromEnum) -- TODO: Partial

-- A '+' followed by an index, an expression and two or more types
sumExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
sumExpr = sumiseI
       \$/ (plus */ natural)
       \*/ (exprI)
       \*/ (typ ?tb)
       \*/ (typ ?tb)
       \*/ (grammarMany $ typ ?tb)
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
productExpr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
productExpr = productiseI
           \$/ (star */ (grammarMany $ exprI))
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
unionExpr
  :: forall b abs tb
   . (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
unionExpr = unioniseI
         \$/ (union */ (typ ?tb))
         \*/ (exprI)
         \*/ (typ ?tb)
         \*/ (typ ?tb)
         \*/ (grammarMany $ (typ ?tb))
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

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Implicits b abs tb
     ,Eq b
     ,Eq abs
     )
  => Grammar (Expr b abs tb)
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
  , betweenParens exprI
  ]

-- Parse an expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. Type)
-- - Type bindings          (E.G. Var)
expr
  :: (Show b
     ,Show abs
     ,Show tb
     ,Ord tb
     ,Eq b
     ,Eq abs
     )
  => Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Grammar (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI

