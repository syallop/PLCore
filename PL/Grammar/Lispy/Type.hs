{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : PL.Grammar.Lispy.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Type with a lisp-like syntax.
-}
module PL.Grammar.Lispy.Type where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import PL.Grammar
import PL.Grammar.Lispy.Kind

import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type hiding (arrowise)

import PL.Iso

tyVar :: Grammar TyVar
tyVar = charIs '?' */ (tyVarI \$/ natural)
  where
    -- TODO: mkTyVar is partial but okay if natural has succeeded
    tyVarI :: Iso Int TyVar
    tyVarI = Iso
      (Just . mkTyVar)
      (\(TyVar v) -> Just . fromEnum $ v)

-- A name is an uppercase followed by zero or more lower case characters
name :: Grammar Text.Text
name = nameI \$/ upper \*/ longestMatching isLower
  where
    nameI :: Iso (Char,Text.Text) Text.Text
    nameI = Iso
      (\(c,cs) -> Just $ Text.cons c cs)
      (\txt -> case Text.uncons txt of
                 Nothing
                   -> Nothing

                 Just (c,cs)
                   | isUpper c -> Just (c,cs)
                   | otherwise -> Nothing
      )

-- A named type is just a name which appears in the type position
namedTyp :: Ord tb => Grammar (Type tb)
namedTyp = namedTypI \$/ name
  where
    namedTypI :: Iso Text.Text (Type tb)
    namedTypI = Iso 
      (Just . Named . TypeName) -- Assumes called with name which performs validation
      (\ty -> case ty of
        Named (TypeName n)
          -> Just n
        _ -> Nothing
      )

-- A '+' followed by zero or more types
sumTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
sumTyp tb = sumTypI \$/ (plus */ grammarMany (typ tb))
  where
    sumTypI :: Iso [Type tb] (Type tb)
    sumTypI = Iso
      (Just . SumT)
      (\t -> case t of
        SumT ts -> Just ts
        _       -> Nothing
      )

-- A '*' followed by zero or more types
productTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
productTyp tb = productTypI \$/ (star */ grammarMany (typ tb))
  where
    productTypI :: Iso [Type tb] (Type tb)
    productTypI = Iso
      (Just . ProductT)
      (\t -> case t of
        ProductT ts -> Just ts
        _           -> Nothing
      )

-- A 'U' followed by zero or more types
unionTyp :: forall tb. (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
unionTyp tb = unionTypI \$/ (union */ grammarMany (typ tb))
  where
    unionTypI :: Iso [Type tb] (Type tb)
    unionTypI = Iso
      (Just . UnionT . Set.fromList)
      (\t -> case t of
        UnionT s -> Just . Set.toList $ s
        _        -> Nothing
      )

-- A '^' followed by two or more types
arrowTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
arrowTyp tb = arrowiseI \$/ (arrow */ typ tb) \*/ typ tb \*/ grammarMany (typ tb)
  where
    -- Iso between a Type and its 'arrowised' form where we have a from, a to
    -- and zero or many trailing types
    arrowiseI :: Iso (Type tb,(Type tb,[Type tb])) (Type tb)
    arrowiseI = Iso
      (\(from,(to,ts)) -> case ts of
          []     -> Just $ Arrow from to
          (t:ts) -> case parseIso arrowiseI (to,(t,ts)) of
                      Nothing
                        -> Nothing

                      Just t
                        -> Just . Arrow from . Arrow to $ t
      )
      (\t -> case t of
               Arrow t ts
                 -> case printIso arrowiseI ts of
                      Just (t0,(t1,t2s))
                        -> Just (t,(t0,t1:t2s))
                      Nothing
                        -> Just (t,(ts,[]))
               _
                 -> Nothing
      )

-- A "/\" followed by an abstracted kind, then a type
typeLamTyp :: forall tb. (Ord tb,Show tb) => Grammar tb -> Grammar (Type tb)
typeLamTyp tb = lamiseI \$/ (bigLambda */ kindAbs) \*/ grammarMany kindAbs \*/ typ tb
  where
    lamiseI :: Iso (Kind,([Kind],Type tb)) (Type tb)
    lamiseI = Iso
      (\(k0,(ks,t)) -> Just $ lamise k0 ks t)

      (\t -> case t of
        TypeLam k0 t
          -> case printIso lamiseI t of
               Just (k,(ks,t))
                 -> Just (k0,(k:ks,t))
               Nothing
                 -> Just (k0,([],t))
        _ -> Nothing
      )

    -- chain lambda
    lamise :: Ord tb => Kind -> [Kind] -> Type tb -> Type tb
    lamise k0 []     t = TypeLam k0 t
    lamise k0 (k:ks) t = TypeLam k0 $ lamise k ks t

-- An "@@" followed by two or more types
typeAppTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typeAppTyp tb = appiseI \$/ (bigAt */ typ tb) \*/ typ tb \*/ grammarMany (typ tb)
  where
    appiseI :: Iso (Type tb,(Type tb,[Type tb])) (Type tb)
    appiseI = Iso
      (\(t0,(t1,t2s)) -> Just $ appise t0 t1 t2s)

      (\t -> case t of
        TypeApp t0 t1
          -> case printIso appiseI t1 of
               Just (t1,(t2,t3s))
                 -> Just (t0,(t1,t2:t3s))

               Nothing
                 -> Just (t0,(t1,[]))
        _ -> Nothing
      )

    appise :: Type tb -> Type tb -> [Type tb] -> Type tb
    appise f x []     = TypeApp f x
    appise f x (y:ys) = appise (TypeApp f x) y ys

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Show tb => Grammar tb -> Grammar (Type tb)
typeBindingTyp gtb = typeBindingI \$/ gtb
  where
    typeBindingI :: Iso tb (Type tb)
    typeBindingI = Iso
      (Just . TypeBinding)
      (\t -> case t of
        TypeBinding tb
          -> Just tb
        _ -> Nothing
      )

-- A type is one of several variants, and may be nested in parenthesis
typ :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typ tb = alternatives
  [typeLamTyp tb
  ,typeAppTyp tb
  ,arrowTyp tb
  ,sumTyp tb
  ,productTyp tb
  ,unionTyp tb
  ,namedTyp
  ,typeBindingTyp tb
  ,betweenParens $ typ tb
  ]

