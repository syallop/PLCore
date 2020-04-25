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

Maps names to types allowing type resolution.
-}
module PL.TypeCtx
  ( TypeCtx ()
  , emptyTypeCtx
  , typeCtxMapping

  , Rec (..)
  , TypeInfo (..)
  , mkTypeInfo

  , lookupTypeNameInfo
  , lookupTypeNameInitialInfo
  , traceLookupTypeNameInitialInfo
  , insertType
  , unionTypeCtx
  , insertRecType

  , resolveTypeInfo
  , resolveTypeInitialInfo
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
  deriving Show

instance Document Rec where
  document r = case r of
    Rec -> text "Rec"
    NonRec -> text "NonRec"

-- Associate typenames to a description of their type
-- Types may be aliased to refer to other types but should only
-- do so a finite amount of times.
newtype TypeCtx phase = TypeCtx {_unTypeCtx :: Map.Map TypeName (TypeInfo phase)}

deriving instance
  (Show (TypeInfo phase))
  => Show (TypeCtx phase)

-- Retrieve the underlying map of type names to their information
typeCtxMapping :: TypeCtx phase -> Map.Map TypeName (TypeInfo phase)
typeCtxMapping = _unTypeCtx

instance Semigroup (TypeCtx phase) where
  (TypeCtx t0) <> (TypeCtx t1) = TypeCtx (t0 <> t1)

instance Monoid (TypeCtx phase) where
  mempty = TypeCtx mempty

instance (Document (TypeFor phase), Document (TypeBindingFor phase)) => Document (TypeCtx phase) where
  document (TypeCtx m) = mconcat . Map.foldrWithKey
                                   (\typeName typeInfo acc -> document typeName : lineBreak : indent 2 (document typeInfo) : lineBreak : lineBreak : acc)
                                   []
                                 $ m

-- Information associated with a type
data TypeInfo phase
  = TypeInfo
    {_typeInfoIsRecursive :: Rec           -- Is the type recursively defined?
    ,_typeInfoKind        :: Kind          -- Cache the Kind of the Type
    ,_typeInfoType        :: TypeFor phase -- The type definition
    }

deriving instance
  (Show (TypeFor phase))
  => Show (TypeInfo phase)

instance (Document (TypeFor phase), Document (TypeBindingFor phase)) => Document (TypeInfo phase) where
  document (TypeInfo isRecursive kind def) = mconcat
    [ document isRecursive
    , lineBreak
    , text ":: ", document kind
    , lineBreak
    , text "= ", document def
    ]

-- Create TypeInfo which MUST be non-recursive
-- TODO: Move 'typeKind' from Expr module, breaking the recursion between Type,Expr,TypeCtx,etc
--       Then actually perform a kind check here instead of cheating
mkTypeInfo :: TypeFor phase -> TypeCtx phase -> TypeInfo phase
mkTypeInfo ty ctx = TypeInfo NonRec kind ty
  where
    {-kind = error "kind unchecked"-}
    kind = Kind

-- Create TypeInfo which is recursive.
mkRecTypeInfo :: TypeFor phase -> TypeCtx phase -> TypeInfo phase
mkRecTypeInfo ty ctx = TypeInfo Rec kind ty
  where
    {-kind = error "kind unchecked"-}
    kind = Kind


-- The empty TypeCtx with no mappings
emptyTypeCtx :: TypeCtx phase
emptyTypeCtx = TypeCtx Map.empty

-- Lookup the TypeInfo associated with a TypeName
lookupTypeNameInfo :: TypeName -> TypeCtx phase -> Maybe (TypeInfo phase)
lookupTypeNameInfo n (TypeCtx ctx) = Map.lookup n ctx

-- Recursively lookup up the TypeInfo associated with a name until a non-'named' definition is found.
-- E.G. If Number = Nat
--         Nat    = SumT [ProductT [],Named "Nat"]
-- Then
-- "lookupTypeNameInfo Number" would return TypeInfo for the type "Named 'Nat'"
-- "lookupTypeNameInitialInfo Number" would recurse twice and return TypeInfo for the "SumT ..." type definition
lookupTypeNameInitialInfo :: TypeName -> TypeCtx phase -> Maybe (TypeInfo phase)
lookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Just (TypeInfo rec kind (NamedExt _ n1))
    -> lookupTypeNameInitialInfo n1 ctx
  t -> t


-- As with "lookupTypeNameInitialInfo" but trace each intermediate TypeInfo looked up to find the intitial info.
traceLookupTypeNameInitialInfo
  :: (NamedExtension phase ~ Void)
  => TypeName
  -> TypeCtx phase
  -> Maybe (TypeInfo phase,[TypeInfo phase])
traceLookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Nothing
    -> Nothing

  Just (TypeInfo rec kind (NamedExt _ n1))
    -> do (t,ns) <- traceLookupTypeNameInitialInfo n1 ctx
          Just (t,TypeInfo rec kind (Named n1) : ns)

  Just ti
    -> Just (ti,[])


-- If a Named type, then lookup associated TypeInfo.
-- otherwise generate it.
resolveTypeInfo :: TypeFor phase -> TypeCtx phase -> Maybe (TypeInfo phase)
resolveTypeInfo t ctx = case t of
  NamedExt _ n -> lookupTypeNameInfo n ctx
  t       -> Just . mkTypeInfo t $ ctx


-- If a Named type, then recursively lookup the associated TypeInfo.
-- otherwise generate it.
resolveTypeInitialInfo :: TypeFor phase -> TypeCtx phase -> Maybe (TypeInfo phase)
resolveTypeInitialInfo t ctx = case t of
  NamedExt _ n -> lookupTypeNameInitialInfo n ctx
  t       -> Just . mkTypeInfo t $ ctx



-- If a Named type, then recursively lookup the associated TypeInfo, also returning a trace of each intermediate TypeInfo.
-- otherwise, generate it first.
traceResolveTypeInitialInfo
  :: (NamedExtension phase ~ Void)
  => TypeFor phase
  -> TypeCtx phase
  -> Maybe (TypeInfo phase,[TypeInfo phase])
traceResolveTypeInitialInfo t ctx = case t of
  NamedExt _ n -> traceLookupTypeNameInitialInfo n ctx
  t       -> Just (mkTypeInfo t ctx,[])


-- Insert a Non-recursive type into the context with a given name, only if
-- - The name is free
-- - All mentioned names are already defined
-- - The type kind-checks
insertType
  :: TypeName
  -> TypeFor phase
  -> TypeCtx phase
  -> Maybe (TypeCtx phase)
insertType n t ctx = case lookupTypeNameInfo n ctx of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something if all names in the type are already defined
  Nothing
    | validDefinition t ctx -> Just . TypeCtx . Map.insert n (mkTypeInfo t ctx) $ _unTypeCtx ctx
    | otherwise             -> Nothing


-- Insert a recursive type into the context with a given name, only if
-- - The name is free
-- - All mentioned names are already defined/ are the name we're attempting to insert
-- - The type kind-checks.
-- TODO: Should a type still be inserted as recursive if it doesnt recurse??
insertRecType :: TypeName -> TypeFor phase -> TypeCtx phase -> Maybe (TypeCtx phase)
insertRecType n t tCtx0 = case lookupTypeNameInfo n tCtx0 of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something
  -- if all names in the type are already defined
  -- OR refer to the type itself
  Nothing
    | validDefinition t tCtx1 -> Just tCtx1
    | otherwise               -> Nothing
    where tCtx1 = let TypeCtx ctx = tCtx0 in TypeCtx $ Map.insert n (mkRecTypeInfo t tCtx0) ctx


-- Validate that a type definition only refers to names that are in the TypeCtx
validDefinition
  :: TypeFor phase
  -> TypeCtx phase
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

unionTypeCtx :: TypeCtx phase -> TypeCtx phase -> TypeCtx phase
unionTypeCtx (TypeCtx t0) (TypeCtx t1) = TypeCtx (Map.union t0 t1)

