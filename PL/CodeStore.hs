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

  -- Shorten and resolve hashes to make them easier for humans to work with
  , shortenExprHash
  , shortenTypeHash
  , shortenKindHash

  , largerExprHashes
  , largerTypeHashes
  , largerKindHashes
  )
  where

import Prelude hiding (lookup)

import PL.Error
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

-- TODO: Define CodeStore -> IO (Either (Error) (CodeStore,a)) Type. Monad
-- instance would reduce a lot of copy-paste.

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
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, StoreResult Expr, Hash))
storeExpr codeStore expr = do
  eRes <- storeByHash (_exprStore codeStore) SHA512 expr
  pure $ case eRes of
    Left err
      -> Left err

    Right (exprStore',res, exprHash)
      -> Right (codeStore{_exprStore = exprStore'}, res, exprHash)

-- | Store a type by it's Hash.
--
-- Pass to lookupType to retrieve.
storeType
  :: CodeStore
  -> Type
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, StoreResult Type, Hash))
storeType codeStore typ = do
  eRes <- storeByHash (_typeStore codeStore) SHA512 typ
  pure $ case eRes of
    Left err
      -> Left err

    Right (typeStore',res, typeHash)
      -> Right (codeStore{_typeStore = typeStore'}, res, typeHash)

-- | Store a kind by it's Hash.
--
-- Pass to lookupKind to retrieve.
storeKind
  :: CodeStore
  -> Kind
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, StoreResult Kind, Hash))
storeKind codeStore kind = do
  eRes <- storeByHash (_kindStore codeStore) SHA512 kind
  pure $ case eRes of
    Left err
      -> Left err

    Right (kindStore',res, kindHash)
      -> Right (codeStore{_kindStore = kindStore'}, res, kindHash)

-- | Lookup an Expr by it's Hash.
--
-- Hashes may be acquired from a prior storeExpr.
lookupExpr
  :: CodeStore
  -> Hash
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, Maybe Expr))
lookupExpr codeStore exprHash = do
  eRes <- lookupByHash (_exprStore codeStore) exprHash
  pure $ case eRes of
    Left err
      -> Left err

    Right (exprStore', expr)
      -> Right (codeStore{_exprStore = exprStore'}, expr)

-- | Lookup an Type by it's Hash.
--
-- Hashes may be acquired from a prior storeType.
lookupType
  :: CodeStore
  -> Hash
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, Maybe Type))
lookupType codeStore typeHash = do
  eRes <- lookupByHash (_typeStore codeStore) typeHash
  pure $ case eRes of
    Left err
      -> Left err

    Right (typeStore', typ)
      -> Right (codeStore{_typeStore = typeStore'}, typ)

-- | Lookup an Kind by it's Hash.
--
-- Hashes may be acquired from a prior storeKind.
lookupKind
  :: CodeStore
  -> Hash
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, Maybe Kind))
lookupKind codeStore kindHash = do
  eRes <- lookupByHash (_kindStore codeStore) kindHash
  pure $ case eRes of
    Left err
      -> Left err

    Right (kindStore', kind)
      -> Right (codeStore{_kindStore = kindStore'}, kind)

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
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, StoreResult Hash))
storeExprHasType codeStore (exprHash,typeHash) = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- store exprTypeStore exprHash typeHash
          pure $ case eRes of
            Left err
              -> Left err

            Right (exprTypeStore',res)
              -> Right (CodeStore exprStore typeStore kindStore exprTypeStore' typeKindStore, res)

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
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, StoreResult Hash))
storeTypeHasKind codeStore (typeHash,kindHash) = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- store typeKindStore typeHash kindHash
          pure $ case eRes of
            Left err
              -> Left err

            Right (typeKindStore',res)
              -> Right (CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore', res)

-- | Given an Expr Hash (perhaps obtained by storeExpr), lookup the associated
-- Type Hash (perhaps stored by storeExprHasType).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupExprType
  :: CodeStore
  -> Hash
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, Maybe Hash))
lookupExprType codeStore exprHash = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- lookup exprTypeStore exprHash
          pure $ case eRes of
            Left err
              -> Left err

            Right (exprTypeStore',exprType)
              -> Right (CodeStore exprStore typeStore kindStore exprTypeStore' typeKindStore, exprType)

-- | Given an Type Hash (perhaps obtained by storeType), lookup the associated
-- Kind Hash (perhaps stored by storeTypeHasKind).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupTypeKind
  :: CodeStore
  -> Hash
  -> IO (Either (Error expr typ pattern typectx) (CodeStore, Maybe Hash))
lookupTypeKind codeStore typeHash = case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- lookup typeKindStore typeHash
          pure $ case eRes of
            Left err
              -> Left err

            Right (typeKindStore',typeKind)
              -> Right (CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore', typeKind)

-- | Given an Expr Hash, shorten it to the shortest unambiguous ShortHash.
shortenExprHash
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, ShortHash))
shortenExprHash codeStore exprHash = case codeStore of
  CodeStore exprStore _typeStore _kindStore _exprTypeStore _typeKindStore
    -> do eRes <- shortenHash exprStore exprHash
          pure $ case eRes of
            Nothing
              -> Nothing

            Just (exprStore', shortHash)
              -> Just (codeStore{_exprStore = exprStore'}, shortHash)

-- | Given a Type Hash, shorten it to the shortest unambiguous ShortHash.
shortenTypeHash
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, ShortHash))
shortenTypeHash codeStore typeHash = case codeStore of
  CodeStore _exprStore typeStore _kindStore _exprTypeStore _typeKindStore
    -> do mRes <- shortenHash typeStore typeHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (typeStore', shortHash)
              -> Just (codeStore{_typeStore = typeStore'}, shortHash)

-- | Given a Kind Hash, shorten it to the shortest unambiguous ShortHash.
shortenKindHash
  :: CodeStore
  -> Hash
  -> IO (Maybe (CodeStore, ShortHash))
shortenKindHash codeStore kindHash = case codeStore of
  CodeStore _exprStore _typeStore kindStore _exprTypeStore _typeKindStore
    -> do mRes <- shortenHash kindStore kindHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (kindStore', shortHash)
              -> Just (codeStore{_kindStore = kindStore'}, shortHash)

-- | Given a short, potentially ambiguous hash for an Expr, gather all larger
-- Hashes.
largerExprHashes
  :: CodeStore
  -> ShortHash
  -> IO (Maybe (CodeStore, [Hash]))
largerExprHashes codeStore shortExprHash = case codeStore of
  CodeStore exprStore _typeStore _kindStore _exprTypeStore _typeKindStore
    -> do mRes <- largerHashes exprStore shortExprHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (exprStore', exprHashes)
              -> Just (codeStore{_exprStore = exprStore'}, exprHashes)

-- | Given a short, potentially ambiguous hash for a Type, gather all larger
-- Hashes.
largerTypeHashes
  :: CodeStore
  -> ShortHash
  -> IO (Maybe (CodeStore, [Hash]))
largerTypeHashes codeStore shortTypeHash = case codeStore of
  CodeStore _exprStore typeStore _kindStore _exprTypeStore _typeKindStore
    -> do mRes <- largerHashes typeStore shortTypeHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (typeStore', typeHashes)
              -> Just (codeStore{_typeStore = typeStore'}, typeHashes)

-- | Given a short, potentially ambiguous hash for a Kind, gather all larger
-- Hashes.
largerKindHashes
  :: CodeStore
  -> ShortHash
  -> IO (Maybe (CodeStore, [Hash]))
largerKindHashes codeStore shortKindHash = case codeStore of
  CodeStore _exprStore _typeStore kindStore _exprTypeStore _typeKindStore
    -> do mRes <- largerHashes kindStore shortKindHash
          pure $ case mRes of
            Nothing
              -> Nothing

            Just (kindStore', kindHashes)
              -> Just (codeStore{_kindStore = kindStore'}, kindHashes)

