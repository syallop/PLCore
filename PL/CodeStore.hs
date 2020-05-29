{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.CodeStore
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Low-level api for storing and retrieving expressions, types and kinds along with
some simple associations such as expressions type and types kind.

Many return values are untyped Hashes that the caller may then choose to resolve.

Resolved Expr,Type,Kinds _are_ verified for correctness when looked up.

Associations are not verified. I.E. They may relate things which don't exist,
or the relation they claim may not be true.

E.G. storeExprHasType (exprHash,typeHash)
Does not verify:
- Expr exists
- Type exists
- Expr has type

Convenience functions are not yet provided for:
- Storing an entire expression alongwith its type and kind
- Automatically resolving returned hashes to their content

The interface currently only supports looking up the 'one' side of 'one-many' relationships.
E.G. You may lookup an expressions type but not all expressions with a type.
-}
module PL.CodeStore
  ( CodeStore ()
  , newCodeStore

  -- Store (Expr,Type,Kind)s by their Hash.
  , storeExpr
  , storeType
  , storeKind

  -- Retrieve (Expr,Type,Kind)s by their Hash.
  , lookupExpr
  , lookupType
  , lookupKind

  -- Store 'hastype'/ 'haskind' relations.
  , storeExprHasType
  , storeTypeHasKind

  -- Lookup the type/ kind of an expr/ type.
  , lookupExprType
  , lookupTypeKind
  )
  where

import Prelude hiding (lookup)

import PL.Store
import PL.ShortStore
import PL.Hash
import PL.HashStore
import PL.Serialize
import PL.Expr
import PL.Type
import PL.Kind

-- | A CodeStore co-ordinates lookup and storage of:
-- - Expressions
-- - Types
-- - Kinds
--
-- As well as some limited associations such as:
-- - Expressions Type
-- - Types Kind
--
-- Construct by passing storage implementations to newCodeStore.
data CodeStore = forall s. (Store s Hash Hash, Show (s Hash Hash)) => CodeStore
  {_exprStore :: HashStore Expr
  ,_typeStore :: HashStore Type
  ,_kindStore :: HashStore Kind

  ,_exprTypeStore :: s Hash Hash
  ,_typeKindStore :: s Hash Hash
  }

-- | Construct a new CodeStore with the provided backing storage.
--
-- It is the callers responsibility to ensure these do not interfere with each
-- other, which could be possible E.G. if a FileStore is used for each and care
-- is not taken when generating subdirectories/ file names.
newCodeStore
  :: ( HashBackingStorage s Expr
     , HashBackingStorage s Type
     , HashBackingStorage s Kind
     , HashBackingStorage s Hash
     , Show (s Hash Expr)
     , Show (s Hash Type)
     , Show (s Hash Kind)
     , Show (s Hash Hash)
     )
  => s Hash Expr
  -> s Hash Type
  -> s Hash Kind
  -> s Hash Hash
  -> s Hash Hash
  -> CodeStore
newCodeStore exprStore typeStore kindStore exprTypeStore typeKindStore = CodeStore
  { _exprStore = newHashStore exprStore
  , _typeStore = newHashStore typeStore
  , _kindStore = newHashStore kindStore

  , _exprTypeStore = exprTypeStore
  , _typeKindStore = typeKindStore
  }

-- | Store an expression by it's Hash.
--
-- Pass to lookupExpr to retrieve.
storeExpr
  :: CodeStore
  -> Expr
  -> IO (Maybe (CodeStore, StoreResult Expr, Hash))
storeExpr codeStore expr = do
  mRes <- storeByHash (_exprStore codeStore) SHA512 expr
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (exprStore',res, exprHash)
      -> Just (codeStore{_exprStore = exprStore'}, res, exprHash)

-- | Store a type by it's Hash.
--
-- Pass to lookupType to retrieve.
storeType
  :: CodeStore
  -> Type
  -> IO (Maybe (CodeStore, StoreResult Type, Hash))
storeType codeStore typ = do
  mRes <- storeByHash (_typeStore codeStore) SHA512 typ
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (typeStore',res, typeHash)
      -> Just (codeStore{_typeStore = typeStore'}, res, typeHash)

-- | Store a kind by it's Hash.
--
-- Pass to lookupKind to retrieve.
storeKind
  :: CodeStore
  -> Kind
  -> IO (Maybe (CodeStore, StoreResult Kind, Hash))
storeKind codeStore kind = do
  mRes <- storeByHash (_kindStore codeStore) SHA512 kind
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (kindStore',res, kindHash)
      -> Just (codeStore{_kindStore = kindStore'}, res, kindHash)

-- | Lookup an Expr by it's Hash.
--
-- Hashes may be acquired from a prior storeExpr.
lookupExpr
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, Expr))
lookupExpr codeStore exprHash = do
  mRes <- lookupByHash (_exprStore codeStore) exprHash
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (exprStore', expr)
      -> Just (codeStore{_exprStore = exprStore'}, expr)

-- | Lookup an Type by it's Hash.
--
-- Hashes may be acquired from a prior storeType.
lookupType
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, Type))
lookupType codeStore typeHash = do
  mRes <- lookupByHash (_typeStore codeStore) typeHash
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (typeStore', typ)
      -> Just (codeStore{_typeStore = typeStore'}, typ)

-- | Lookup an Kind by it's Hash.
--
-- Hashes may be acquired from a prior storeKind.
lookupKind
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, Kind))
lookupKind codeStore kindHash = do
  mRes <- lookupByHash (_kindStore codeStore) kindHash
  pure $ case mRes of
    Nothing
      -> Nothing

    Just (kindStore', kind)
      -> Just (codeStore{_kindStore = kindStore'}, kind)

-- | Store a promise that in (exprHash,typeHash):
-- - exprHash resolves to a well-typed expression
-- - typeHash resolves to a well-kinded type
-- - The expression has the given type.
--
-- None of these properties are checked. It is the callers responsibility not to
-- poison the store.
storeExprHasType
  :: CodeStore
  -> (Hash,Hash)
  -> IO (Maybe (CodeStore, StoreResult Hash))
storeExprHasType codeStore (exprHash,typeHash) = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do mRes <- store exprTypeStore exprHash typeHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (exprTypeStore',res)
              -> Just (CodeStore exprStore typeStore kindStore exprTypeStore' typeKindStore, res)

-- | Store a promise that in (typeHash,kindHash):
-- - typeHash resolves to a well-kinded type
-- - kindHash resolves to a kind
-- - The type has the given kind
--
-- None of these properties are checked. It is the callers responsibility not to
-- poison the store.
storeTypeHasKind
  :: CodeStore
  -> (Hash,Hash)
  -> IO (Maybe (CodeStore, StoreResult Hash))
storeTypeHasKind codeStore (typeHash,kindHash) = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do mRes <- store typeKindStore typeHash kindHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (typeKindStore',res)
              -> Just (CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore', res)

-- | Given an Expr Hash (perhaps obtained by storeExpr), lookup the associated
-- Type Hash (perhaps stored by storeExprHasType).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupExprType
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, Hash))
lookupExprType codeStore exprHash = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do mRes <- lookup exprTypeStore exprHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (exprTypeStore',exprType)
              -> Just (CodeStore exprStore typeStore kindStore exprTypeStore' typeKindStore, exprType)

-- | Given an Type Hash (perhaps obtained by storeType), lookup the associated
-- Kind Hash (perhaps stored by storeTypeHasKind).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupTypeKind
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, Hash))
lookupTypeKind codeStore typeHash = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do mRes <- lookup typeKindStore typeHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (typeKindStore',typeKind)
              -> Just (CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore', typeKind)

