{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module PL.Binds where

import PL.Type
import PL.Binds.Ix

import PL.Printer

import Data.Monoid

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


  toList :: BindCtx b ty -> [(b,ty)]

showBindCtx :: (Show b,Show ty,Binds b ty) => BindCtx b ty -> String
showBindCtx = (++ "]") . ("[" ++) . concat . foldr (\(b,ty) acc -> (show b ++ ":" ++ show ty):acc) [] . toList

instance (Document b,Document ty, Binds b ty)
      => Document (BindCtx b ty) where
  document = between (char '[',char ']') . foldr (\(b,ty) acc -> mconcat [document b,":",document ty,acc]) emptyDoc . toList

instance (Show b,Show ty,Binds b ty) => Show (BindCtx b ty) where
  show = showBindCtx

