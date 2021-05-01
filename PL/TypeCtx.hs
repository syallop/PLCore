{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , StandaloneDeriving
  , GADTs
  , TypeFamilies
  #-}
{-|
Module      : PL.TypeCtx
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a TypeCtx datastructure which can be used to map TypeNames
to definitions and their metadata.

I.E. Lookups can return:
- Kinds - useful for deciding whether a name can be applied to a type or not.
- Recursive/ non-recursive - useful for determining a reduction strategy that
  will terminate.
- The type's definition - useful for substitution & comparing structural
  equality.

Not to be confused with:
- Binds which maps lambda abstractions in expressions/ types to metadata (such as a variables type or kind)
- Bindings allows looking up bound/ unbound bindings to an abstraction (such as a variables expression or a type-variables type)

In its current usage, type contexts are hardcoded to some default types
(Nat,Bool,Maybe,etc) and threaded through evaluation.

At some point the ability to define types will be exposed to the user either:
- Out of band
- At the top-level
- Anywhere in an expression

Depending that choice as well as decisions on:
- Naming
- Type uniqueness
- Recursion
- Generalisation to named expression
definitions here will be replaced/ made stronger. Due to the amount of unknowns,
this api isn't particularly finished or as safe as it should be and so care
should be taken when using to avoid creating bad states.
-}
module PL.TypeCtx
  ( TypeCtx
  , TypeCtxFor ()
  , emptyTypeCtx
  , typeCtxMapping
  , ppTypeCtx

  , lookupTypeNameType
  , lookupTypeNameInitialType
  , insertType
  , unionTypeCtx
  , insertRecType

  , resolveTypeInfo
  )
  where

import PL.Kind
import PL.Name
import PL.Type
import PL.FixPhase
import PL.Error

import PLPrinter

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Associate type names to a description of their type.
--
-- Types may be aliased to refer to other types but there should not be cycles
-- unless they are denoted as being 'Rec'ursive.
type TypeCtx = TypeCtxFor DefaultPhase

newtype TypeCtxFor phase = TypeCtx {_unTypeCtx :: Map.Map TypeName (TypeFor phase)}

deriving instance Show (TypeFor phase) => Show (TypeCtxFor phase)
deriving instance Eq (TypeFor phase) => Eq (TypeCtxFor phase)
deriving instance Ord (TypeFor phase) => Ord (TypeCtxFor phase)


-- Retrieve the underlying map of type names to their information
typeCtxMapping
  :: TypeCtxFor phase
  -> Map.Map TypeName (TypeFor phase)
typeCtxMapping = _unTypeCtx

instance Semigroup (TypeCtxFor phase) where
  (TypeCtx t0) <> (TypeCtx t1) = TypeCtx (t0 <> t1)

instance Monoid (TypeCtxFor phase) where
  mempty = TypeCtx mempty

instance Document (TypeFor phase)
  => Document (TypeCtxFor phase)
  where
  document = ppTypeCtx document document

ppTypeCtx
  :: (TypeName -> Doc)
  -> (TypeFor phase -> Doc)
  -> TypeCtxFor phase
  -> Doc
ppTypeCtx ppTypeName ppType (TypeCtx m) =
  Map.foldrWithKey
    (\name info acc
      -> mconcat [ ppTypeName name
                 , lineBreak
                 , indent 2 $ ppType info
                 , lineBreak
                 , lineBreak
                 , acc
                 ]
    )
    emptyDoc
    m

-- The empty TypeCtx with no mappings
emptyTypeCtx
  :: TypeCtxFor phase
emptyTypeCtx = TypeCtx Map.empty

-- Lookup the TypeInfo associated with a TypeName
lookupTypeNameType
  :: TypeName
  -> TypeCtxFor phase
  -> Maybe (TypeFor phase)
lookupTypeNameType n (TypeCtx ctx) = Map.lookup n ctx

-- Recursively lookup up the Type associated with a name until a non-'named' definition is found.
-- E.G. If Number = Nat
--         Nat    = SumT [ProductT [],Named "Nat"]
-- Then
-- "lookupTypeNameInfo Number" would return a Type for the type "Named 'Nat'"
-- "lookupTypeNameInitialInfo Number" would recurse twice and return a Type for the "SumT ..." type definition
lookupTypeNameInitialType
  :: TypeName
  -> TypeCtxFor phase
  -> Maybe (TypeFor phase)
lookupTypeNameInitialType n0 ctx = case lookupTypeNameType n0 ctx of
  Just (NamedExt _ n1)
    -> lookupTypeNameInitialType n1 ctx
  t -> t

-- If a Named type, then lookup associated TypeInfo.
resolveTypeInfo
  :: TypeFor phase
  -> TypeCtxFor phase
  -> Maybe (TypeFor phase)
resolveTypeInfo t ctx = case t of
  NamedExt _ n -> lookupTypeNameType n ctx
  _       -> Nothing

-- Insert a Non-recursive type into the context with a given name, only if
-- - The name is free
-- - All mentioned names are already defined
-- - The type kind-checks
insertType
  :: TypeName
  -> TypeFor phase
  -> Kind
  -> TypeCtxFor phase
  -> Maybe (TypeCtxFor phase)
insertType n t _k ctx = case lookupTypeNameType n ctx of
  -- TODO: The supplied kind is ignored

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something if all names in the type are already defined
  Nothing
    | validDefinition t ctx -> Just . TypeCtx . Map.insert n t $ _unTypeCtx ctx
    | otherwise             -> Nothing


-- Insert a recursive type into the context with a given name, only if
-- - The name is free
-- - All mentioned names are already defined/ are the name we're attempting to insert
-- - The type kind-checks.
-- TODO: Should a type still be inserted as recursive if it doesnt recurse??
insertRecType
  :: TypeName
  -> TypeFor phase
  -> Kind
  -> TypeCtxFor phase
  -> Maybe (TypeCtxFor phase)
insertRecType n t k tCtx0 = case lookupTypeNameType n tCtx0 of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something
  -- if all names in the type are already defined
  -- OR refer to the type itself
  Nothing
    | validDefinition t tCtx1 -> Just tCtx1
    | otherwise               -> Nothing
    where tCtx1 = let TypeCtx ctx = tCtx0 in TypeCtx $ Map.insert n (TypeMu k t) ctx


-- Validate that a type definition only refers to names that are in the TypeCtx
validDefinition
  :: TypeFor phase
  -> TypeCtxFor phase
  -> Bool
validDefinition t ctx = case t of
  NamedExt _ n
    -> isJust $ lookupTypeNameType n ctx

  ArrowExt _ from to
    -> validDefinition from ctx && validDefinition to ctx

  SumTExt _ ts
    -> all (`validDefinition` ctx) ts

  ProductTExt _ ts
    -> all (`validDefinition` ctx) ts

  UnionTExt _ ts
    -> all (`validDefinition` ctx) . Set.toList $ ts

  BigArrowExt _ _ toTy
    -> validDefinition toTy ctx

  TypeBindingExt _ _
    -> True

  TypeLamExt _ _ typ
    -> validDefinition typ ctx

  TypeAppExt _ f x
    -> validDefinition f ctx && validDefinition x ctx

  TypeMuExt _ _ itself
    -> validDefinition itself ctx

  TypeContentBindingExt _ _
    -> True

  TypeSelfBindingExt _
    -> True

  _ -> error "Non-exhaustive pattern when checking type definition is valid"

unionTypeCtx
  :: TypeCtxFor phase
  -> TypeCtxFor phase
  -> TypeCtxFor phase
unionTypeCtx (TypeCtx t0) (TypeCtx t1) = TypeCtx (Map.union t0 t1)

type instance ErrorTypeCtx DefaultPhase = TypeCtx
