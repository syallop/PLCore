{-# LANGUAGE
     ConstraintKinds
   , FlexibleContexts
   , FlexibleInstances
   , GADTs
   , GeneralizedNewtypeDeriving
   , LambdaCase
   , MultiParamTypeClasses
   , OverloadedStrings
   , RankNTypes
   , ScopedTypeVariables
   , StandaloneDeriving
   , UndecidableInstances
   , TypeSynonymInstances
   #-}
{-|
Module      : PL.FixExpr
Copyright   : (c) Samuel A. Yallop, 2018
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.FixExpr where

import PLPrinter.Doc

-- | Fix a PL expression type with nested sub-expressions 'expr'.
-- 'b' is the type of bindings.
-- 'abs' is the type of abstractions.
-- 'tb' is the type of type-bindings.
newtype FixExpr b abs tb expr
  = FixExpr {_unFixExpr :: expr b abs tb (FixExpr b abs tb expr)}

deriving instance Show (expr b abs tb (FixExpr b abs tb expr)) => Show (FixExpr b abs tb expr)
deriving instance Eq (expr b abs tb (FixExpr b abs tb expr)) => Eq (FixExpr b abs tb expr)
deriving instance Document (expr b abs tb (FixExpr b abs tb expr)) => Document (FixExpr b abs tb expr)

unfixExpr
  :: FixExpr b abs tb expr
  -> expr b abs tb (FixExpr b abs tb expr)
unfixExpr = _unFixExpr

fixExpr
  :: expr b abs tb (FixExpr b abs tb expr)
  -> FixExpr b abs tb expr
fixExpr = FixExpr

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

-- | Catamorphism. Generic function fold.
cataExpr
  :: Functor (expr b abs tb)
  => (expr b abs tb expr' -> expr')
  -> (FixExpr b abs tb expr -> expr')
cataExpr f = f . fmap (cataExpr f) . _unFixExpr

-- | Anamorphism. Generic function unfold.
anaExpr
  :: Functor (expr b abs tb)
  => (exprA -> expr b abs tb exprA)
  -> (exprA -> FixExpr b abs tb expr)
anaExpr f = FixExpr . fmap (anaExpr f) . f

-- | Hylomorphism. Unfold followed by fold.
hyloExpr
  :: Functor (expr b abs tb)
  => (expr b abs tb exprB -> exprB)
  -> (exprA -> expr b abs tb exprA)
  -> (exprA -> exprB)
hyloExpr phi psi = cataExpr phi . anaExpr psi

-- | Infix hylomorphism.
(~>)
  :: Functor (expr b abs tb)
  => (exprA -> expr b abs tb exprA)
  -> (expr b abs tb exprB -> exprB)
  -> (exprA -> exprB)
psi ~> phi = phi . fmap (hyloExpr phi psi) . psi

-- | Monadic cataExpr.
cataExprM
  :: ( Monad m
     , Traversable (expr b abs tb)
     )
  => (expr b abs tb exprA -> m exprA)
  -> FixExpr b abs tb expr
  -> m exprA
cataExprM f = (f =<<) . traverse (cataExprM f) . _unFixExpr

-- | Monadic anaExpr.
anaExprM
  :: ( Monad m
     , Traversable (expr b abs tb)
     )
  => (exprA -> m (expr b abs tb exprA))
  -> exprA
  -> m (FixExpr b abs tb expr)
anaExprM f = fmap FixExpr . (traverse (anaExprM f) =<<) . f

-- | Monadic hyloExpr.
hyloExprM
  :: ( Monad m
     , Traversable (expr b abs tb)
     )
  => (expr b abs tb exprB -> m exprB)
  -> (exprA -> m (expr b abs tb exprA))
  -> (exprA -> m exprB)
hyloExprM phi psi = (cataExprM phi =<<) . anaExprM psi
