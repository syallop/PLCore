{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module PL.Binds where

import PL.Type

-- 'b' maps to and from an index describing where the variable was bound.
-- The associated ''BindCtx b' associates 'b' to types'

-- A type used in an expression to bind an abstraction to a variable-like thing.
-- I.E. \ABS -> Var BINDS

-- Associate binding types 'b' to types 'ty' in a 'BindCtx'.
--
-- 'b' maps to and from an index describing where the variable was bound.
class BindingIx b => Binds b ty where

  -- Associate bindings to their types
  data BindCtx b ty

  -- Empty context where nothing is bound to a ty
  emptyCtx    :: BindCtx b ty

  -- Lookup the possible ty of a binding
  lookupBindingTy :: b -> BindCtx b ty -> Maybe ty

  -- Add a new bound ty
  addBinding  :: ty -> BindCtx b ty -> BindCtx b ty

  -- Add a list of bound tys in order of furthest to nearest
  addBindings :: [ty] -> BindCtx b ty -> BindCtx b ty
  addBindings ts ctx = foldl (flip addBinding) ctx ts

-- class of index-like binding types, E.G. Var.
-- A binding type maps to an index describing where the abstraction it binds is and may be buries below a number of new abstractions
class BindingIx b where
  -- How far away is the abstraction we're binding?
  bindDepth :: b -> Int

  -- During a reduction, a binding may be buried below a number of new abstractions
  buryBinding :: b -> Int -> b


