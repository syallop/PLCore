{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module PL.Binds where

import PL.Type

-- 'b' maps to and from an index describing where the variable was bound.
-- The associated ''BindCtx b' associates 'b' to types'

-- A type used in an expression to bind an abstraction to a variable-like thing.
-- I.E. \ABS -> Var BINDS
class (Show b,Eq b,Show tb,Eq tb,Binds' b) => Binds b tb where

  -- Associate bindings to their types
  data BindCtx b tb
  emptyCtx    :: BindCtx b tb
  bindTy      :: b -> BindCtx b tb -> Maybe (Type tb)
  addBinding  :: Type tb -> BindCtx b tb -> BindCtx b tb
  addBindings :: [Type tb] -> BindCtx b tb -> BindCtx b tb

class (Show b,Eq b) => Binds' b where
  -- How far away is the abstraction we're binding?
  bindDepth :: b -> Int

  -- During a reduction, a binding may be buried below a number of new abstractions
  buryBinding :: b -> Int -> b


