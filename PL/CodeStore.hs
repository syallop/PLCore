{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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

  , CodeStoreFunction ()
  , runCodeStoreFunction

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
import PL.FixPhase
import PL.Serialize
import PL.Expr
import PL.Type
import PL.Kind

import PLPrinter.Doc

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
  { _exprStore :: HashStore Expr
  , _typeStore :: HashStore Type
  , _kindStore :: HashStore Kind

  , _exprTypeStore :: s Hash Hash
  , _typeKindStore :: s Hash Hash
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

-- | The type of computations that:
-- - Have access to a CodeStore as state
-- - May execute IO
-- - Have short-circuiting Errors
--
-- Run with runCodeStoreFunction
-- Construct a CodeStore with newCodeStore.
newtype CodeStoreFunction a = CodeStoreFunction { _runCodeStoreFunction :: CodeStore -> IO (Either Error (CodeStore, a))}

-- | Execute a CodeStoreFunction to produce either the first error or the result
-- and final CodeStore.
runCodeStoreFunction
  :: CodeStore
  -> CodeStoreFunction a
  -> IO (Either Error (CodeStore, a))
runCodeStoreFunction codeStore f = _runCodeStoreFunction f codeStore

instance Functor CodeStoreFunction where
  fmap f (CodeStoreFunction codeStoreF) = CodeStoreFunction $ \codeStore -> do
    eRes <- codeStoreF codeStore
    case eRes of
      Left err
        -> pure . Left $ err

      Right (codeStore', a)
        -> pure . Right $ (codeStore', f a)

instance Applicative CodeStoreFunction where
  pure a = CodeStoreFunction $ \codeStore -> pure $ Right (codeStore, a)

  -- Perhaps this should be parallel on the initial context
  (CodeStoreFunction fab) <*> (CodeStoreFunction a) = CodeStoreFunction $ \codeStore -> do
    r  <- fab codeStore
    case r of
      Left err
        -> pure . Left $ err

      Right (codeStore', f)
        -> do a' <- a codeStore'
              case a' of
                Left err
                  -> pure . Left $ err

                Right (codeStore'', a'')
                  -> pure . Right $ (codeStore'', f a'')

instance Monad CodeStoreFunction where
  return = pure

  (CodeStoreFunction fa) >>= g = CodeStoreFunction $ \codeStore -> do
    r <- fa codeStore
    case r of
      Left err
        -> pure . Left $ err

      Right (codeStore',a)
        -> let CodeStoreFunction fb = g a
            in fb codeStore'

{- Internal helper functions to reduce chance of mistake modifying
   stores incorrectly.

   TODO: Explore tagging Hashes with the type of thing they hash to make these
   bugs difficult. This would also allow resolving the intended stores by type,
   possibly reducing the api surface area.
-}

-- Inject an error into the CodeStoreFunction
codeStoreFunctionFail
  :: Error
  -> CodeStoreFunction a
codeStoreFunctionFail err = CodeStoreFunction $ \_ -> pure . Left $ err

-- Modify the contained expression store, returning a single result.
withExprStore
  :: (HashStore Expr -> IO (Either Error (HashStore Expr, a)))
  -> CodeStoreFunction a
withExprStore f = CodeStoreFunction $ \codeStore -> do
  eRes <- f (_exprStore codeStore)
  case eRes of
    Left err
      -> pure . Left $ err
    Right (exprStore',a)
      -> pure . Right $ (codeStore{_exprStore = exprStore'}, a)

-- Modify the contained type store, returning a single result.
withTypeStore
  :: (HashStore Type -> IO (Either Error (HashStore Type, a)))
  -> CodeStoreFunction a
withTypeStore f = CodeStoreFunction $ \codeStore -> do
  eRes <- f (_typeStore codeStore)
  case eRes of
    Left err
      -> pure . Left $ err
    Right (typeStore',a)
      -> pure . Right $ (codeStore{_typeStore = typeStore'}, a)

-- Modify the contained kind store, returning a single result.
withKindStore
  :: (HashStore Kind -> IO (Either Error (HashStore Kind, a)))
  -> CodeStoreFunction a
withKindStore f = CodeStoreFunction $ \codeStore -> do
  eRes <- f (_kindStore codeStore)
  case eRes of
    Left err
      -> pure . Left $ err
    Right (kindStore',a)
      -> pure . Right $ (codeStore{_kindStore = kindStore'}, a)

-- Modify the contained expr-has-type store, returning a single result
withExprTypeStore
  :: (forall s. Store s Hash Hash => s Hash Hash -> IO (Either Error (s Hash Hash, a)))
  -> CodeStoreFunction a
withExprTypeStore f = CodeStoreFunction $ \codeStore -> case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- f exprTypeStore
          case eRes of
            Left err
              -> pure . Left $ err
            Right (exprTypeStore',a)
              -> pure . Right $ (CodeStore exprStore typeStore kindStore exprTypeStore' typeKindStore, a)

-- Modify the contained type-has-kind store, retuning a single result
withTypeKindStore
  :: (forall s. Store s Hash Hash => s Hash Hash -> IO (Either Error (s Hash Hash, a)))
  -> CodeStoreFunction a
withTypeKindStore f = CodeStoreFunction $ \codeStore -> case codeStore of
  CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore
    -> do eRes <- f typeKindStore
          case eRes of
            Left err
              -> pure . Left $ err
            Right (typeKindStore',a)
              -> pure . Right $ (CodeStore exprStore typeStore kindStore exprTypeStore typeKindStore', a)

-- Over some HashStore, store a value by its SHA512 hash.
codeHashStore
  :: ((HashStore v -> IO (Either Error (HashStore v, HashStoreResult v))) -> a)
  -> v
  -> a
codeHashStore withHashStore value = withHashStore $ \s -> storeByHash s SHA512 value

-- Over some HashStore, lookup a value by its hash.
codeHashLookup
  :: ((HashStore v -> IO (Either Error (HashStore v, Maybe v))) -> a)
  -> Hash
  -> a
codeHashLookup withHashStore hash = withHashStore $ \s -> lookupByHash s hash

-- Over some Store, store a value against a key.
codeStore
  :: ((forall s. Store s k v => s k v -> IO (Either Error (s k v, StoreResult v))) -> a)
  -> k
  -> v
  -> a
codeStore withStore key value = withStore $ \s -> store s key value

-- Over some Store, lookup a value given it's key.
codeLookup
  :: ((forall s. Store s k v => s k v -> IO (Either Error (s k v, Maybe v))) -> a)
  -> k
  -> a
codeLookup withStore k = withStore $ \s -> lookup s k

-- Over some HashStore, shorten a hash.
codeHashShorten
  :: ((HashStore v -> IO (Either Error (HashStore v, ShortHash))) -> a)
  -> Hash
  -> a
codeHashShorten withHashStore hash = withHashStore $ \s -> shortenHash s hash

-- Over some HashStore, largen a hash.
codeHashLargen
  :: ((HashStore v -> IO (Either Error (HashStore v, [Hash]))) -> a)
  -> ShortHash
  -> a
codeHashLargen withHashStore shortHash = withHashStore $ \s -> largerHashes s shortHash

{- External api -}

-- | Store an expression by it's Hash.
--
-- Pass to lookupExpr to retrieve.
storeExpr
  :: Expr
  -> CodeStoreFunction (HashStoreResult Expr)
storeExpr (ContentBinding name) = codeStoreFunctionFail . EMsg . mconcat $
  [ text "Top-level Content Bindings cannot be stored (as they would replace the expression they reference with themselves!):"
  , lineBreak
  , indent1 . string . show $ name
  ]
storeExpr expr = codeHashStore withExprStore expr

-- | Store a type by it's Hash.
--
-- Pass to lookupType to retrieve.
storeType
  :: Type
  -> CodeStoreFunction (HashStoreResult Type)
storeType (TypeContentBinding name) = codeStoreFunctionFail . EMsg . mconcat $
  [ text "Top-level Type Content Bindings cannot be stored (as they would replace the type they reference with themselves!):"
  , lineBreak
  , indent1 . string . show $ name
  ]
storeType typ = codeHashStore withTypeStore typ

-- | Store a kind by it's Hash.
--
-- Pass to lookupKind to retrieve.
storeKind
  :: Kind
  -> CodeStoreFunction (HashStoreResult Kind)
storeKind = codeHashStore withKindStore

-- | Lookup an Expr by it's Hash.
--
-- Hashes may be acquired from a prior storeExpr.
lookupExpr
  :: Hash
  -> CodeStoreFunction (Maybe Expr)
lookupExpr = codeHashLookup withExprStore

-- | Lookup an Type by it's Hash.
--
-- Hashes may be acquired from a prior storeType.
lookupType
  :: Hash
  -> CodeStoreFunction (Maybe Type)
lookupType = codeHashLookup withTypeStore

-- | Lookup an Kind by it's Hash.
--
-- Hashes may be acquired from a prior storeKind.
lookupKind
  :: Hash
  -> CodeStoreFunction (Maybe Kind)
lookupKind = codeHashLookup withKindStore

-- | Store a promise that in (exprHash,typeHash):
-- - exprHash resolves to a well-typed expression
-- - typeHash resolves to a well-kinded type
-- - The expression has the given type.
--
-- None of these properties are checked. It is the callers responsibility not to
-- poison the store.
storeExprHasType
  :: Hash
  -> Hash
  -> CodeStoreFunction (StoreResult Hash)
storeExprHasType = codeStore withExprTypeStore

-- | Store a promise that in (typeHash,kindHash):
-- - typeHash resolves to a well-kinded type
-- - kindHash resolves to a kind
-- - The type has the given kind
--
-- None of these properties are checked. It is the callers responsibility not to
-- poison the store.
storeTypeHasKind
  :: Hash
  -> Hash
  -> CodeStoreFunction (StoreResult Hash)
storeTypeHasKind = codeStore withTypeKindStore

-- | Given an Expr Hash (perhaps obtained by storeExpr), lookup the associated
-- Type Hash (perhaps stored by storeExprHasType).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupExprType
  :: Hash
  -> CodeStoreFunction (Maybe Hash)
lookupExprType = codeLookup withExprTypeStore

-- | Given an Type Hash (perhaps obtained by storeType), lookup the associated
-- Kind Hash (perhaps stored by storeTypeHasKind).
--
-- It is the callers responsibility to validate whether the association is true
-- if the backing storage is not reliable.
lookupTypeKind
  :: Hash
  -> CodeStoreFunction (Maybe Hash)
lookupTypeKind = codeLookup withTypeKindStore

-- | Given an Expr Hash, shorten it to the shortest unambiguous ShortHash.
shortenExprHash
  :: Hash
  -> CodeStoreFunction ShortHash
shortenExprHash = codeHashShorten withExprStore

-- | Given a Type Hash, shorten it to the shortest unambiguous ShortHash.
shortenTypeHash
  :: Hash
  -> CodeStoreFunction ShortHash
shortenTypeHash = codeHashShorten withTypeStore

-- | Given a Kind Hash, shorten it to the shortest unambiguous ShortHash.
shortenKindHash
  :: Hash
  -> CodeStoreFunction ShortHash
shortenKindHash = codeHashShorten withKindStore

-- | Given a short, potentially ambiguous hash for an Expr, gather all larger
-- Hashes.
largerExprHashes
  :: ShortHash
  -> CodeStoreFunction [Hash]
largerExprHashes = codeHashLargen withExprStore

-- | Given a short, potentially ambiguous hash for a Type, gather all larger
-- Hashes.
largerTypeHashes
  :: ShortHash
  -> CodeStoreFunction [Hash]
largerTypeHashes = codeHashLargen withTypeStore

-- | Given a short, potentially ambiguous hash for a Kind, gather all larger
-- Hashes.
largerKindHashes
  :: ShortHash
  -> CodeStoreFunction [Hash]
largerKindHashes = codeHashLargen withKindStore

