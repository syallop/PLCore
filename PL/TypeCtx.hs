{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
  , StandaloneDeriving
  , GADTs
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

  , Rec (..)
  , TypeInfo
  , TypeInfoFor (..)
  , mkTypeInfo
  , ppTypeInfo

  , lookupTypeNameInfo
  , lookupTypeNameInitialInfo
  , traceLookupTypeNameInitialInfo
  , insertType
  , unionTypeCtx
  , insertRecType

  , resolveTypeInfo
  , traceResolveTypeInitialInfo
  )
  where

import PL.Binds
import PL.Kind
import PL.Name
import PL.Type
import PL.FixPhase

import PLPrinter

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Rec describes whether a type recurses on itself or not.
data Rec
  = Rec
  | NonRec
  deriving (Show, Eq, Ord)

instance Document Rec where
  document r = case r of
    Rec -> text "Rec"
    NonRec -> text "NonRec"

-- | Associate type names to a description of their type.
--
-- Types may be aliased to refer to other types but there should not be cycles
-- unless they are denoted as being 'Rec'ursive.
type TypeCtx = TypeCtxFor DefaultPhase

newtype TypeCtxFor phase = TypeCtx {_unTypeCtx :: Map.Map TypeName (TypeInfoFor phase)}

deriving instance
  (Show (TypeInfoFor phase))
  => Show (TypeCtxFor phase)

deriving instance
  (Ord (TypeInfoFor phase))
  => Ord (TypeCtxFor phase)

deriving instance
  (Eq (TypeInfoFor phase))
  => Eq (TypeCtxFor phase)

-- Retrieve the underlying map of type names to their information
typeCtxMapping
  :: TypeCtxFor phase
  -> Map.Map TypeName (TypeInfoFor phase)
typeCtxMapping = _unTypeCtx

instance Semigroup (TypeCtxFor phase) where
  (TypeCtx t0) <> (TypeCtx t1) = TypeCtx (t0 <> t1)

instance Monoid (TypeCtxFor phase) where
  mempty = TypeCtx mempty

instance Document (TypeFor phase)
  => Document (TypeCtxFor phase)
  where
  document = ppTypeCtx document (ppTypeInfo document)

ppTypeCtx
  :: (TypeName -> Doc)
  -> (TypeInfoFor phase -> Doc)
  -> TypeCtxFor phase
  -> Doc
ppTypeCtx ppTypeName ppTypeInfo (TypeCtx m) =
  Map.foldrWithKey
    (\typeName typeInfo acc
      -> mconcat [ ppTypeName typeName
                 , lineBreak
                 , indent 2 $ ppTypeInfo typeInfo
                 , lineBreak
                 , lineBreak
                 , acc
                 ]
    )
    emptyDoc
    m

-- | Information associated with a type that can be used to provide hints to
-- type-checking, reduction phases.
--
-- - Caching the Types Kind makes type-checking TypeApplications faster.
-- - Storing whether the definition is recursive or not allows reduction to
--   avoid non-termination.
-- - The definition of the type itself can be used to compare structural
--   equality.
type TypeInfo = TypeInfoFor DefaultPhase

data TypeInfoFor phase
  = TypeInfo
    {_typeInfoIsRecursive :: Rec
    ,_typeInfoKind        :: Kind
    ,_typeInfoType        :: TypeFor phase
    }

deriving instance
  (Show (TypeFor phase))
  => Show (TypeInfoFor phase)

deriving instance
  (Ord (TypeFor phase))
  => Ord (TypeInfoFor phase)

deriving instance
  (Eq (TypeFor phase))
  => Eq (TypeInfoFor phase)

instance
  ( Document (TypeFor phase)
  , Document (TypeBindingFor phase)
  ) => Document (TypeInfoFor phase) where
  document = ppTypeInfo document

ppTypeInfo
  :: (TypeFor phase -> Doc)
  -> TypeInfoFor phase
  -> Doc
ppTypeInfo ppDef (TypeInfo isRecursive kind def) = mconcat
  [ document isRecursive
  , lineBreak
  , text ":: ", document kind
  , lineBreak
  , text "= ", ppDef def
  ]

-- Create TypeInfo which MUST be non-recursive
mkTypeInfo
  :: TypeFor phase
  -> Kind
  -> TypeCtxFor phase
  -> TypeInfoFor phase
mkTypeInfo ty kind ctx = TypeInfo NonRec kind ty

-- Create TypeInfo which is recursive.
mkRecTypeInfo
  :: TypeFor phase
  -> Kind
  -> TypeCtxFor phase
  -> TypeInfoFor phase
mkRecTypeInfo ty kind ctx = TypeInfo Rec kind ty

-- The empty TypeCtx with no mappings
emptyTypeCtx
  :: TypeCtxFor phase
emptyTypeCtx = TypeCtx Map.empty

-- Lookup the TypeInfo associated with a TypeName
lookupTypeNameInfo
  :: TypeName
  -> TypeCtxFor phase
  -> Maybe (TypeInfoFor phase)
lookupTypeNameInfo n (TypeCtx ctx) = Map.lookup n ctx

-- Recursively lookup up the TypeInfo associated with a name until a non-'named' definition is found.
-- E.G. If Number = Nat
--         Nat    = SumT [ProductT [],Named "Nat"]
-- Then
-- "lookupTypeNameInfo Number" would return TypeInfo for the type "Named 'Nat'"
-- "lookupTypeNameInitialInfo Number" would recurse twice and return TypeInfo for the "SumT ..." type definition
lookupTypeNameInitialInfo
  :: TypeName
  -> TypeCtxFor phase
  -> Maybe (TypeInfoFor phase)
lookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Just (TypeInfo rec kind (NamedExt _ n1))
    -> lookupTypeNameInitialInfo n1 ctx
  t -> t


-- As with "lookupTypeNameInitialInfo" but trace each intermediate TypeInfo looked up to find the intitial info.
traceLookupTypeNameInitialInfo
  :: (NamedExtension phase ~ Void)
  => TypeName
  -> TypeCtxFor phase
  -> Maybe (TypeInfoFor phase,[TypeInfoFor phase])
traceLookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Nothing
    -> Nothing

  Just (TypeInfo rec kind (NamedExt _ n1))
    -> do (t,ns) <- traceLookupTypeNameInitialInfo n1 ctx
          Just (t,TypeInfo rec kind (Named n1) : ns)

  Just ti
    -> Just (ti,[])

-- If a Named type, then lookup associated TypeInfo.
resolveTypeInfo
  :: TypeFor phase
  -> TypeCtxFor phase
  -> Maybe (TypeInfoFor phase)
resolveTypeInfo t ctx = case t of
  NamedExt _ n -> lookupTypeNameInfo n ctx
  _       -> Nothing

-- If a Named type, then recursively lookup the associated TypeInfo, also returning a trace of each intermediate TypeInfo.
traceResolveTypeInitialInfo
  :: (NamedExtension phase ~ Void)
  => TypeFor phase
  -> TypeCtxFor phase
  -> Maybe (TypeInfoFor phase,[TypeInfoFor phase])
traceResolveTypeInitialInfo t ctx = case t of
  NamedExt _ n
    -> traceLookupTypeNameInitialInfo n ctx

  _
    -> Nothing


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
insertType n t k ctx = case lookupTypeNameInfo n ctx of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something if all names in the type are already defined
  Nothing
    | validDefinition t ctx -> Just . TypeCtx . Map.insert n (mkTypeInfo t k ctx) $ _unTypeCtx ctx
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
insertRecType n t k tCtx0 = case lookupTypeNameInfo n tCtx0 of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something
  -- if all names in the type are already defined
  -- OR refer to the type itself
  Nothing
    | validDefinition t tCtx1 -> Just tCtx1
    | otherwise               -> Nothing
    where tCtx1 = let TypeCtx ctx = tCtx0 in TypeCtx $ Map.insert n (mkRecTypeInfo t k tCtx0) ctx


-- Validate that a type definition only refers to names that are in the TypeCtx
validDefinition
  :: TypeFor phase
  -> TypeCtxFor phase
  -> Bool
validDefinition t ctx = case t of
  NamedExt _ n
    -> isJust $ lookupTypeNameInfo n ctx

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

  _ -> error "Non-exhaustive pattern when checking type definition is valid"

unionTypeCtx
  :: TypeCtxFor phase
  -> TypeCtxFor phase
  -> TypeCtxFor phase
unionTypeCtx (TypeCtx t0) (TypeCtx t1) = TypeCtx (Map.union t0 t1)

