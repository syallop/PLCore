{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , UndecidableInstances
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
import PL.FixType

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
newtype TypeCtx tb = TypeCtx {_unTypeCtx :: Map.Map TypeName (TypeInfo tb)}
  deriving Show

instance Semigroup (TypeCtx tb) where
  (TypeCtx t0) <> (TypeCtx t1) = TypeCtx (t0 <> t1)

instance Monoid (TypeCtx tb) where
  mempty = TypeCtx mempty

instance (Document (Type tb), Document tb) => Document (TypeCtx tb) where
  document (TypeCtx m) = mconcat . Map.foldrWithKey
                                   (\typeName typeInfo acc -> document typeName : lineBreak : indent 2 (document typeInfo) : lineBreak : lineBreak : acc)
                                   []
                                 $ m

-- Information associated with a type
data TypeInfo tb
  = TypeInfo
    {_typeInfoIsRecursive :: Rec     -- Is the type recursively defined?
    ,_typeInfoKind        :: Kind    -- Cache the Kind of the Type
    ,_typeInfoType        :: Type tb -- The type definition
    }
  deriving Show

instance (Document (Type tb), Document tb) => Document (TypeInfo tb) where
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
mkTypeInfo :: Type tb -> TypeCtx tb -> TypeInfo tb
mkTypeInfo ty ctx = TypeInfo NonRec kind ty
  where
    {-kind = error "kind unchecked"-}
    kind = Kind

-- Create TypeInfo which is recursive.
mkRecTypeInfo :: Type tb -> TypeCtx tb -> TypeInfo tb
mkRecTypeInfo ty ctx = TypeInfo Rec kind ty
  where
    {-kind = error "kind unchecked"-}
    kind = Kind


-- The empty TypeCtx with no mappings
emptyTypeCtx :: TypeCtx tb
emptyTypeCtx = TypeCtx Map.empty

-- Lookup the TypeInfo associated with a TypeName
lookupTypeNameInfo :: TypeName -> TypeCtx tb -> Maybe (TypeInfo tb)
lookupTypeNameInfo n (TypeCtx ctx) = Map.lookup n ctx

-- Recursively lookup up the TypeInfo associated with a name until a non-'named' definition is found.
-- E.G. If Number = Nat
--         Nat    = SumT [ProductT [],Named "Nat"]
-- Then
-- "lookupTypeNameInfo Number" would return TypeInfo for the type "Named 'Nat'"
-- "lookupTypeNameInitialInfo Number" would recurse twice and return TypeInfo for the "SumT ..." type definition
lookupTypeNameInitialInfo :: TypeName -> TypeCtx tb -> Maybe (TypeInfo tb)
lookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Just (TypeInfo rec kind (FixType (Named n1)))
    -> lookupTypeNameInitialInfo n1 ctx
  t -> t


-- As with "lookupTypeNameInitialInfo" but trace each intermediate TypeInfo looked up to find the intitial info.
traceLookupTypeNameInitialInfo :: TypeName -> TypeCtx tb -> Maybe (TypeInfo tb,[TypeInfo tb])
traceLookupTypeNameInitialInfo n0 ctx = case lookupTypeNameInfo n0 ctx of
  Nothing
    -> Nothing

  Just (TypeInfo rec kind (FixType (Named n1)))
    -> do (t,ns) <- traceLookupTypeNameInitialInfo n1 ctx
          Just (t,TypeInfo rec kind (fixType $ Named n1) : ns)

  Just ti
    -> Just (ti,[])


-- If a Named type, then lookup associated TypeInfo.
-- otherwise generate it.
resolveTypeInfo :: Type tb -> TypeCtx tb -> Maybe (TypeInfo tb)
resolveTypeInfo t ctx = case unfixType t of
  Named n -> lookupTypeNameInfo n ctx
  t       -> Just . mkTypeInfo (fixType t) $ ctx


-- If a Named type, then recursively lookup the associated TypeInfo.
-- otherwise generate it.
resolveTypeInitialInfo :: Type tb -> TypeCtx tb -> Maybe (TypeInfo tb)
resolveTypeInitialInfo t ctx = case unfixType t of
  Named n -> lookupTypeNameInitialInfo n ctx
  t       -> Just . mkTypeInfo (fixType t) $ ctx



-- If a Named type, then recursively lookup the associated TypeInfo, also returning a trace of each intermediate TypeInfo.
-- otherwise, generate it first.
traceResolveTypeInitialInfo :: Type tb -> TypeCtx tb -> Maybe (TypeInfo tb,[TypeInfo tb])
traceResolveTypeInitialInfo t ctx = case unfixType t of
  Named n -> traceLookupTypeNameInitialInfo n ctx
  t       -> Just (mkTypeInfo (fixType t) ctx,[])


-- Insert a Non-recursive type into the context with a given name, only if
-- - The name is free
-- - All mentioned names are already defined
-- - The type kind-checks
insertType :: TypeName -> Type tb -> TypeCtx tb -> Maybe (TypeCtx tb)
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
insertRecType :: TypeName -> Type tb -> TypeCtx tb -> Maybe (TypeCtx tb)
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
validDefinition :: Type tb -> TypeCtx tb -> Bool
validDefinition t ctx = case unfixType t of
  Named n
    -> isJust $ lookupTypeNameInfo n ctx

  Arrow from to
    -> validDefinition from ctx && validDefinition to ctx

  SumT ts
    -> all (`validDefinition` ctx) ts

  ProductT ts
    -> all (`validDefinition` ctx) ts

  UnionT ts
    -> all (`validDefinition` ctx) . Set.toList $ ts

  BigArrow _ toTy
    -> validDefinition toTy ctx

  TypeBinding _
    -> True

  TypeLam _ typ
    -> validDefinition typ ctx

  TypeApp f x
    -> validDefinition f ctx && validDefinition x ctx

unionTypeCtx :: TypeCtx tb -> TypeCtx tb -> TypeCtx tb
unionTypeCtx (TypeCtx t0) (TypeCtx t1) = TypeCtx (Map.union t0 t1)

