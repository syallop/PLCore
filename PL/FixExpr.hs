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
newtype FixExpr phase expr
  = FixExpr {_unFixExpr :: expr phase (FixExpr phase expr)}

deriving instance Show (expr phase (FixExpr phase expr)) => Show (FixExpr phase expr)
deriving instance Eq (expr phase (FixExpr phase expr)) => Eq (FixExpr phase expr)
deriving instance Document (expr phase (FixExpr phase expr)) => Document (FixExpr phase expr)

unfixExpr
  :: FixExpr phase expr
  -> expr phase (FixExpr phase expr)
unfixExpr = _unFixExpr

fixExpr
  :: expr phase (FixExpr phase expr)
  -> FixExpr phase expr
fixExpr = FixExpr

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

-- | Catamorphism. Generic function fold.
cataExpr
  :: Functor (expr phase)
  => (expr phase expr' -> expr')
  -> (FixExpr phase expr -> expr')
cataExpr f = f . fmap (cataExpr f) . _unFixExpr

-- | Anamorphism. Generic function unfold.
anaExpr
  :: Functor (expr phase)
  => (exprA -> expr phase exprA)
  -> (exprA -> FixExpr phase expr)
anaExpr f = FixExpr . fmap (anaExpr f) . f

-- | Hylomorphism. Unfold followed by fold.
hyloExpr
  :: Functor (expr phase)
  => (expr phase exprB -> exprB)
  -> (exprA -> expr phase exprA)
  -> (exprA -> exprB)
hyloExpr phi psi = cataExpr phi . anaExpr psi

-- | Infix hylomorphism.
(~>)
  :: Functor (expr phase)
  => (exprA -> expr phase exprA)
  -> (expr phase exprB -> exprB)
  -> (exprA -> exprB)
psi ~> phi = phi . fmap (hyloExpr phi psi) . psi

-- | Monadic cataExpr.
cataExprM
  :: ( Monad m
     , Traversable (expr phase)
     )
  => (expr phase exprA -> m exprA)
  -> FixExpr phase expr
  -> m exprA
cataExprM f = (f =<<) . traverse (cataExprM f) . _unFixExpr

-- | Monadic anaExpr.
anaExprM
  :: ( Monad m
     , Traversable (expr phase)
     )
  => (exprA -> m (expr phase exprA))
  -> exprA
  -> m (FixExpr phase expr)
anaExprM f = fmap FixExpr . (traverse (anaExprM f) =<<) . f

-- | Monadic hyloExpr.
hyloExprM
  :: ( Monad m
     , Traversable (expr phase)
     )
  => (expr phase exprB -> m exprB)
  -> (exprA -> m (expr phase exprA))
  -> (exprA -> m exprB)
hyloExprM phi psi = (cataExprM phi =<<) . anaExprM psi
