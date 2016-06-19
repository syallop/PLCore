{-# LANGUAGE MultiParamTypeClasses #-}
module PL.ExprLike where

-- Class of 'e'xpression-like types which abstract over themselves.
class HasAbs e where
  -- Apply a function to abstracted sub 'e'xpression
  applyToAbs :: (e -> e) -> e -> e

class HasBinding e b where
  applyToBinding :: (b -> b) -> e -> e

-- Class of 'e'xpressions which have non-abstracted sub terms
class HasNonAbs e where
  applyToNonAbs :: (e -> e) -> e -> e

