module PL.TypeCtx
  (TypeCtx()
  ,emptyTypeCtx
  ,lookupType
  ,lookupInitialType
  ,traceLookupInitialType
  ,insertType
  ,unionTypeCtx
  ,typeEq
  ,typeEqs
  ,insertRecType

  ,resolveType
  ,resolveInitialType
  ,traceResolveInitialType
  )
  where

import PL.Name
import PL.Type

import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

data Rec
  = Rec
  | NonRec
  deriving Show

-- Associate typenames to a description of their type
-- Types may be aliased to refer to other types but should only
-- do so a finite amount of times.
newtype TypeCtx tb = TypeCtx {_unTypeCtx :: Map.Map TypeName (Type tb,Rec)}
  deriving Show

emptyTypeCtx :: TypeCtx tb
emptyTypeCtx = TypeCtx Map.empty

lookupType :: TypeName -> TypeCtx tb -> Maybe (Type tb)
lookupType n (TypeCtx ctx) = fst <$> Map.lookup n ctx

-- Recursively look up a type name until a non-'named' type is found.
lookupInitialType :: TypeName -> TypeCtx tb -> Maybe (Type tb)
lookupInitialType n0 ctx = case lookupType n0 ctx of
  Just (Named n1) -> lookupInitialType n1 ctx
  t               -> t

-- recursively look up  a type name until a non-'named' type is found
-- also returning a list of the intermediate names.
traceLookupInitialType :: TypeName -> TypeCtx tb -> Maybe (Type tb,[Type tb])
traceLookupInitialType n0 ctx = case lookupType n0 ctx of
  Nothing         -> Nothing
  Just (Named n1) -> do (t,ns) <- traceLookupInitialType n1 ctx
                        pure (t,Named n1:ns)
  Just t -> undefined

-- Lookup a Named type or return the type
resolveType :: Type tb -> TypeCtx tb -> Maybe (Type tb)
resolveType t tCtx = case t of
  Named n -> lookupType n tCtx
  _       -> Just t

-- Recursively lookup a Named type until a non-'named' type is found.
resolveInitialType :: Type tb -> TypeCtx tb -> Maybe (Type tb)
resolveInitialType t tCtx = case t of
  Named n  -> lookupInitialType n tCtx
  _        -> Just t

-- recursively look up a Named type until a non-'named' type is found
-- also returning a list of the intermediate names.
traceResolveInitialType :: Type tb -> TypeCtx tb -> Maybe (Type tb,[Type tb])
traceResolveInitialType t tCtx = case t of
  Named n -> traceLookupInitialType n tCtx
  _       -> Just (t,[])


insertType :: TypeName -> Type tb -> TypeCtx tb -> Maybe (TypeCtx tb)
insertType n t ctx = case lookupType n ctx of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something
  -- if all names in the type are already defined
  Nothing
    | validDefinition t ctx -> Just . TypeCtx . Map.insert n (t,NonRec) $ _unTypeCtx ctx
    | otherwise             -> Nothing

-- Insert a type whose definition may refer to itself
insertRecType :: TypeName -> Type tb -> TypeCtx tb -> Maybe (TypeCtx tb)
insertRecType n t tCtx0 = case lookupType n tCtx0 of

  -- Name is already associated with something
  Just _ -> Nothing

  -- Name is free to be associated with something
  -- if all names in the type are already defined
  -- OR refer to the type itself
  Nothing
    | validDefinition t tCtx1 -> Just tCtx1
    | otherwise               -> Nothing
    where tCtx1 = let TypeCtx ctx = tCtx0 in TypeCtx $ Map.insert n (t,Rec) ctx

-- Validate that a type definition only refers to names that are in the TypeCtx
validDefinition :: Type tb -> TypeCtx tb -> Bool
validDefinition t ctx = case t of
  Named n
    -> isJust $ lookupType n ctx

  Arrow from to
    -> validDefinition from ctx && validDefinition to ctx

  SumT ts
    -> and $ map (`validDefinition` ctx) ts

  ProductT ts
    -> and $ map (`validDefinition` ctx) ts

  UnionT ts
    -> and . map (`validDefinition` ctx) $ Set.toList ts

  TypeLam _ typ
    -> validDefinition typ ctx

  TypeApp f x
    -> validDefinition f ctx && validDefinition x ctx

unionTypeCtx :: TypeCtx tb -> TypeCtx tb -> TypeCtx tb
unionTypeCtx (TypeCtx t0) (TypeCtx t1) = TypeCtx (Map.union t0 t1)

-- Are two types equivalent under a typectx?
-- TODO: Should check equality under a BindCtx tb Kind
typeEq :: Type tb -> Type tb -> TypeCtx tb -> Maybe Bool
typeEq t0 t1 tCtx = case (t0,t1) of
  (Named n0,Named n1)
    | n0 == n1  -> Just True
    | otherwise -> do it0 <- lookupInitialType n0 tCtx
                      it1 <- lookupInitialType n1 tCtx
                      typeEq it0 it1 tCtx

  (Named n0,_)
    -> do it0 <- lookupInitialType n0 tCtx
          typeEq it0 t1 tCtx

  (_,Named n1)
    -> do it1 <- lookupInitialType n1 tCtx
          typeEq it1 t0 tCtx

  (Arrow from0 to0,Arrow from1 to1)
    -> (&&) <$> typeEq from0 from1 tCtx <*> typeEq to0 to1 tCtx

  (SumT ts0,SumT ts1)
    -> typeEqs ts0 ts1 tCtx

  (ProductT ts0,ProductT ts1)
    -> typeEqs ts0 ts1 tCtx

  (UnionT ts0,UnionT ts1)
    -> typeEqs (Set.toList ts0) (Set.toList ts1) tCtx

  (TypeLam k0 ty0,TypeLam k1 ty1)
    -> (&&) <$> pure (k0 == k1) <*> typeEq ty0 ty1 tCtx

  (TypeApp f0 x0,TypeApp f1 x1)
    -> (&&) <$> typeEq f0 f1 tCtx <*> typeEq x0 x1 tCtx

  -- A non-Named, non identical type
  _ -> Just False

-- Are two lists of types pairwise equivalent under a typectx?
typeEqs :: [Type tb] -> [Type tb] -> TypeCtx tb -> Maybe Bool
typeEqs ts0 ts1 tCtx
  | length ts0 == length ts1 = fmap and $ mapM (\(t0, t1) -> typeEq t0 t1 tCtx) $ zip ts0 ts1
  | otherwise                = Just False

