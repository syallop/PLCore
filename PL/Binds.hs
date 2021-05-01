{-# LANGUAGE
    MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , FlexibleContexts
  #-}
{-|
Module      : PL.Binds
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Class of things that can be used to map bindings to metadata.
This abstraction should likely be removed/ merged with Bindings which is confusingly similar.

In short:
Binds:
- Maps binding constructs (such as variables, type variables) to metadata such
  as the type for variables and the kind for type variables.
- Is used in the typechecking/ kind checking phase before reduction as the
   interface does not allow for bindings to be moved under each other.
Whereas Bindings:
- Map Binding indexes (which variables, type variables may be/ contain) to
  either their bound value, or an Unbound token.
- Is used in the reduction/ evaluation phase as the interface allows for
  bindings to be moved under each other as functions are applied.
-}
module PL.Binds where

import PL.Binds.Ix
import PLPrinter hiding (between)
import PLPrinter.Doc

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

showBindCtx
  :: (Show b,Show ty,Binds b ty)
  => BindCtx b ty
  -> String
showBindCtx = (++ "]") . ("[" ++) . concat . foldr (\(b,ty) acc -> (show b ++ ":" ++ show ty):acc) [] . toList

ppBindCtx
  :: Binds b ty
  => (b -> Doc)
  -> (ty -> Doc)
  -> BindCtx b ty
  -> Doc
ppBindCtx ppB ppTy = between (char '[') (char ']') . foldr (\(b,ty) acc -> mconcat [ppB b, text ":", ppTy ty,acc]) emptyDoc . toList

instance (Document b,Document ty, Binds b ty)
      => Document (BindCtx b ty) where
  document = ppBindCtx document document

instance (Show b,Show ty,Binds b ty) => Show (BindCtx b ty) where
  show = showBindCtx

