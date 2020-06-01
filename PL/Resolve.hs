{-# LANGUAGE
    GADTs
  , ConstraintKinds
  , LambdaCase
  , RankNTypes
  , FlexibleContexts
  , OverloadedStrings
  #-}
{-|
Module      : PL.Resolve
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

This module defines a compiler phase which can be used to resolve ShortHashes
within {expr,type,pattern}s to unambiguous Hashes in the context of some CodeStore.

Resolution would typically take place after parsing and before type-checking.

-}
module PL.Resolve
  ( ResolveCtx (..)
  , mkResolveCtx

  , Resolve ()
  , runResolve

  -- Resolve ShortHashes within {expr,type,pattern}s to unambigous Hashes
  , resolveShortHashes
  , resolveTypeShortHashes
  , resolvePatternShortHashes

  , resolveCaseShortHashes
  , resolveCaseBranchesShortHashes
  , resolveCaseBranchShortHashes

  -- Resolve individual hashes
  , resolveExprHash
  , resolveTypeHash
  , resolveKindHash

  -- Claim that two phases are identical with one using ShortHashes and the
  -- other resolved Hashes.
  , ExprWithResolvedHashes
  , TypeWithResolvedHashes
  , PatternWithResolvedHashes
  )
  where

import PL.CodeStore
import PL.Error
import PL.Expr
import PL.Case
import PL.Pattern
import PL.Type
import PL.TypeCtx
import PL.Name
import PL.Hash
import PL.HashStore

import PLPrinter.Doc

import qualified Data.Set as Set

-- | Contains information used when resolving names within expressions, types
-- and patterns.
data ResolveCtx = ResolveCtx
  { _resolveCodeStore :: CodeStore -- ^ Backing CodeStore consulted to map ShortHashes to Hashes.
  }

-- | Create a ResolveCtx with a CodeStore that can be consulted as the authority
-- on whether ShortHashes are prefixes of any larger Hashes.
mkResolveCtx
  :: CodeStore
  -> ResolveCtx
mkResolveCtx codeStore = ResolveCtx
  { _resolveCodeStore = codeStore
  }

-- | The type of resolution computations that:
-- - Have access to a ResolveCtx as state
-- - May execute IO
-- - Have short-circuiting Errors
--
-- Run with runResolve
-- Construct a context either from mkResolveCtx or manually.
newtype Resolve a = Resolve { _resolve :: ResolveCtx -> IO (Either (Error Expr Type Pattern TypeCtx) (ResolveCtx,a)) }

-- | Execute a Resolve function to produce either the first error or the result
-- and the final context.
runResolve
  :: ResolveCtx
  -> Resolve a
  -> IO (Either (Error Expr Type Pattern TypeCtx) (ResolveCtx, a))
runResolve ctx r = _resolve r ctx

instance Functor Resolve where
  fmap f (Resolve resolve) = Resolve $ \ctx -> do
    r <- resolve ctx
    case r of
      Left err
        -> pure $ Left err

      Right (ctx',a)
        -> pure $ Right (ctx', f a)

instance Applicative Resolve where
  pure a = Resolve $ \ctx -> pure $ Right (ctx, a)

  -- Perhaps this should be parallel on the initial context
  (Resolve fab) <*> (Resolve a) = Resolve $ \ctx -> do
    r  <- fab ctx
    case r of
      Left err
        -> pure $ Left err
      Right (ctx', f)
        -> do a' <- a ctx'
              case a' of
                Left err
                  -> pure $ Left err
                Right (ctx'',a'')
                  -> pure $ Right (ctx'', f a'')

instance Monad Resolve where
  return = pure

  (Resolve fa) >>= fAToResolveB = Resolve $ \ctx -> do
    r <- fa ctx
    case r of
      Left err
        -> pure $ Left err

      Right (ctx',a)
        -> let Resolve fb = fAToResolveB a
            in fb ctx'

-- | Attempt to resolve a ShortHash for an expression to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
resolveExprHash
  :: ShortHash
  -> Resolve Hash
resolveExprHash shortHash = Resolve $ \ctx -> do
  mRes <- largerExprHashes (_resolveCodeStore ctx) shortHash
  let errCtx = EContext (EMsg . mconcat $ [text "Resolving expression short-hash:"
                                          ,lineBreak
                                          ,indent1 . string . show $ shortHash
                                          ])
  case mRes of
    Nothing
      -> pure . Left . errCtx . EMsg . text $ "Unknown error in underlying code store"

    Just (codeStore', hashes)
      -> case hashes of
           []
             -> pure . Left . errCtx . EMsg . text $ "No known matching hashes"

           [hash]
             -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, hash)

           hashes
             -> pure . Left . errCtx . EMsg . mconcat $
                 [ text "More than one hash with the given prefix was found. did you mean one of these?"
                 , string . show $ hashes
                 ]

-- | Attempt to resolve a ShortHash for a type to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
resolveTypeHash
  :: ShortHash
  -> Resolve Hash
resolveTypeHash shortHash = Resolve $ \ctx -> do
  mRes <- largerTypeHashes (_resolveCodeStore ctx) shortHash
  let errCtx = EContext (EMsg . mconcat $ [text "Resolving type short-hash:"
                                          ,lineBreak
                                          ,indent1 . string . show $ shortHash
                                          ])
  case mRes of
    Nothing
      -> pure . Left . errCtx . EMsg . text $ "Unknown error in underlying code store"

    Just (codeStore', hashes)
      -> case hashes of
           []
             -> pure . Left . errCtx . EMsg . text $ "No known matching hashes"

           [hash]
             -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, hash)

           hashes
             -> pure . Left . errCtx . EMsg . mconcat $
                 [ text "More than one hash with the given prefix was found. did you mean one of these?"
                 , string . show $ hashes
                 ]

-- | Attempt to resolve a ShortHash for a kind to an unambiguous Hash.
--
-- It is an error if:
-- - There is no known larger Hash
-- - There are multiple colliding Hashes
resolveKindHash
  :: ShortHash
  -> Resolve Hash
resolveKindHash shortHash = Resolve $ \ctx -> do
  mRes <- largerKindHashes (_resolveCodeStore ctx) shortHash
  let errCtx = EContext (EMsg . mconcat $ [text "Resolving kind short-hash:"
                                          ,lineBreak
                                          ,indent1 . string . show $ shortHash
                                          ])
  case mRes of
    Nothing
      -> pure . Left . errCtx . EMsg . text $ "Unknown error in underlying code store"

    Just (codeStore', hashes)
      -> case hashes of
           []
             -> pure . Left . errCtx . EMsg . text $ "No known matching hashes"

           [hash]
             -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, hash)

           hashes
             -> pure . Left . errCtx . EMsg . mconcat $
                 [ text "More than one hash with the given prefix was found. did you mean one of these?"
                 , string . show $ hashes
                 ]

-- | Two expression phases are identical but:
-- - The first uses potentially ambiguous ShortHashes
-- - The second uses unambiguous Hashes
type ExprWithResolvedHashes unresolved resolved =
  ( ContentBindingFor unresolved  ~ ShortHash
  , ContentBindingFor resolved    ~ ContentName

  , PatternWithResolvedHashes unresolved resolved
  , TypeWithResolvedHashes    unresolved resolved

  , Ord (ExprFor unresolved)
  , Ord (TypeFor resolved)

    -- Otherwise, both expressions have the same extensions
  , LamExtension            unresolved ~ LamExtension            resolved
  , AppExtension            unresolved ~ AppExtension            resolved
  , BindingExtension        unresolved ~ BindingExtension        resolved
  , ContentBindingExtension unresolved ~ ContentBindingExtension resolved
  , CaseAnalysisExtension   unresolved ~ CaseAnalysisExtension   resolved
  , SumExtension            unresolved ~ SumExtension            resolved
  , ProductExtension        unresolved ~ ProductExtension        resolved
  , UnionExtension          unresolved ~ UnionExtension          resolved
  , BigLamExtension         unresolved ~ BigLamExtension         resolved
  , BigAppExtension         unresolved ~ BigAppExtension         resolved
  , ExprExtension           unresolved ~ ExprExtension           resolved

    -- Both expressions use the same binding and types.
  , BindingFor        unresolved ~ BindingFor        resolved
  , TypeBindingFor    unresolved ~ TypeBindingFor    resolved
  )

-- | Two type phases are identical but:
-- - The first uses potentially ambiguous ShortHashes
-- - The second uses unambiguous Hashes
type TypeWithResolvedHashes unresolved resolved =
  ( TypeContentBindingFor unresolved ~ ShortHash
  , TypeContentBindingFor resolved   ~ ContentName

  , Ord (TypeFor resolved)

    -- Otherwise both types have the same extensions
  , NamedExtension              unresolved ~ NamedExtension              resolved
  , ArrowExtension              unresolved ~ ArrowExtension              resolved
  , SumTExtension               unresolved ~ SumTExtension               resolved
  , ProductTExtension           unresolved ~ ProductTExtension           resolved
  , UnionTExtension             unresolved ~ UnionTExtension             resolved
  , BigArrowExtension           unresolved ~ BigArrowExtension           resolved
  , TypeLamExtension            unresolved ~ TypeLamExtension            resolved
  , TypeAppExtension            unresolved ~ TypeAppExtension            resolved
  , TypeBindingExtension        unresolved ~ TypeBindingExtension        resolved
  , TypeContentBindingExtension unresolved ~ TypeContentBindingExtension resolved
  , TypeExtension               unresolved ~ TypeExtension               resolved

    -- Both types use the same type bindings
  , TypeBindingFor unresolved ~ TypeBindingFor resolved
  )

-- | Two pattern phases are identical but:
-- - The first uses potentially ambiguous ShortHashes
-- - The second uses unambiguous Hashes
type PatternWithResolvedHashes unresolved resolved =
  ( ContentBindingFor unresolved  ~ ShortHash
  , ContentBindingFor resolved    ~ ContentName

  , TypeWithResolvedHashes unresolved resolved

    -- Otherwise both patterns have the same extensions
  , SumPatternExtension     unresolved ~ SumPatternExtension     resolved
  , ProductPatternExtension unresolved ~ ProductPatternExtension resolved
  , UnionPatternExtension   unresolved ~ UnionPatternExtension   resolved
  , BindingPatternExtension unresolved ~ BindingPatternExtension resolved
  , BindExtension           unresolved ~ BindExtension           resolved
  , PatternExtension        unresolved ~ PatternExtension        resolved

    -- Both patterns use the same bindings
  , BindingFor unresolved ~ BindingFor resolved
  )

-- | Attempt to resolve all ShortHashes contained within an expression (it's,
-- patterns and it's types) into unambiguous ContentNames.
resolveShortHashes
  :: ( ExprWithResolvedHashes unresolved resolved
     , AbstractionFor unresolved ~ TypeFor typePhase
     , AbstractionFor resolved   ~ TypeFor typePhase
     )
  => ExprFor unresolved
  -> Resolve (ExprFor resolved)
resolveShortHashes e = case e of
  ContentBindingExt ext shortHash
    -> ContentBindingExt ext . mkContentName <$> resolveExprHash shortHash

  LamExt ext abs bodyExpr
    -> LamExt ext abs <$> resolveShortHashes bodyExpr

  AppExt ext fExpr xExpr
    -> AppExt ext <$> resolveShortHashes fExpr <*> resolveShortHashes xExpr

  CaseAnalysisExt ext c
    -> CaseAnalysisExt ext <$> resolveCaseShortHashes c

  SumExt ext expr ix ty
    -> SumExt ext <$> resolveShortHashes expr <*> pure ix <*> mapM resolveTypeShortHashes ty

  ProductExt ext prodExprs
    -> ProductExt ext <$> mapM resolveShortHashes prodExprs

  UnionExt ext unionExpr tyIx ty
    -> UnionExt ext <$> resolveShortHashes unionExpr
                    <*> resolveTypeShortHashes tyIx
                    <*> (fmap Set.fromList $ mapM resolveTypeShortHashes $ Set.toList ty)

  BindingExt ext b
    -> pure $ BindingExt ext b

  BigLamExt ext takeTy expr
    -> BigLamExt ext takeTy <$> resolveShortHashes expr

  BigAppExt ext fExpr xTy
    -> BigAppExt ext <$> resolveShortHashes fExpr <*> resolveTypeShortHashes xTy

  ExprExtensionExt ext
    -> pure $ ExprExtensionExt ext

  _ -> error "Unimplemented"

-- | Attempt to resolve all ShortHashes contained within a case statement to
-- unambigous ContentNames.
resolveCaseShortHashes
  :: ( ExprWithResolvedHashes unresolved resolved
     , AbstractionFor unresolved ~ TypeFor typePhase
     , AbstractionFor resolved   ~ TypeFor typePhase
     )
  => Case (ExprFor unresolved) (PatternFor unresolved)
  -> Resolve (Case (ExprFor resolved) (PatternFor resolved))
resolveCaseShortHashes (Case expr branches) = Case <$> resolveShortHashes expr <*> resolveCaseBranchesShortHashes branches

-- | Attempt to resolve all ShortHashes contained within case branches to
-- unambigous ContentNames.
resolveCaseBranchesShortHashes
  :: ( ExprWithResolvedHashes unresolved resolved
     , AbstractionFor unresolved ~ TypeFor typePhase
     , AbstractionFor resolved   ~ TypeFor typePhase
     )
  => CaseBranches (ExprFor unresolved) (PatternFor unresolved)
  -> Resolve (CaseBranches (ExprFor resolved) (PatternFor resolved))
resolveCaseBranchesShortHashes = \case
  DefaultOnly expr
    -> DefaultOnly <$> resolveShortHashes expr

  CaseBranches neBranches mDefault
    -> CaseBranches <$> mapM resolveCaseBranchShortHashes neBranches <*> mapM resolveShortHashes mDefault

-- | Attempt to resolve all ShortHashes contained within a case branch to
-- unambigous ContentNames.
resolveCaseBranchShortHashes
  :: ( ExprWithResolvedHashes unresolved resolved
     , AbstractionFor unresolved ~ TypeFor typePhase
     , AbstractionFor resolved   ~ TypeFor typePhase
     )
  => CaseBranch (ExprFor unresolved) (PatternFor unresolved)
  -> Resolve (CaseBranch (ExprFor resolved) (PatternFor resolved))
resolveCaseBranchShortHashes (CaseBranch match result) = CaseBranch <$> (resolvePatternShortHashes match) <*> (resolveShortHashes result)

-- | Attempt to resolve all ShortHashes contained within a pattern to
-- unambigous ContentNames.
resolvePatternShortHashes
  :: PatternWithResolvedHashes unresolved resolved
  => PatternFor unresolved
  -> Resolve (PatternFor resolved)
resolvePatternShortHashes = \case
  BindExt ext
    -> pure $ BindExt ext

  BindingPatternExt ext b
    -> pure $ BindingPatternExt ext b

  SumPatternExt ext sumIndex nestedPattern
    -> SumPatternExt ext sumIndex <$> resolvePatternShortHashes nestedPattern

  ProductPatternExt ext nestedPatterns
    -> ProductPatternExt ext <$> mapM resolvePatternShortHashes nestedPatterns

  UnionPatternExt ext  unionIndexTy nestedPattern
    -> UnionPatternExt ext <$> resolveTypeShortHashes unionIndexTy <*> resolvePatternShortHashes nestedPattern

  PatternExtensionExt ext
    -> pure $ PatternExtensionExt ext

  _ -> error "Non-exhaustive pattern match when add comments from match statement"

-- | Attempt to resolve all ShortHashes contained within a type to
-- unambigous ContentNames.
resolveTypeShortHashes
  :: ( TypeWithResolvedHashes unresolved resolved
     )
  => TypeFor unresolved
  -> Resolve (TypeFor resolved)
resolveTypeShortHashes = \case
  TypeContentBindingExt ext shortHash
    -> TypeContentBindingExt ext . mkContentName <$> resolveTypeHash shortHash

  NamedExt ext tyName
    -> pure $ NamedExt ext tyName

  ArrowExt ext from to
    -> ArrowExt ext <$> resolveTypeShortHashes from <*> resolveTypeShortHashes to

  SumTExt ext types
    -> SumTExt ext <$> mapM resolveTypeShortHashes types

  ProductTExt ext types
    -> ProductTExt ext <$> mapM resolveTypeShortHashes types

  UnionTExt ext types
    -> UnionTExt ext <$> (fmap Set.fromList . mapM resolveTypeShortHashes . Set.toList $ types)

  BigArrowExt ext from to
    -> BigArrowExt ext from <$> resolveTypeShortHashes to

  TypeLamExt ext kind typ
    -> TypeLamExt ext kind <$> resolveTypeShortHashes typ

  TypeAppExt ext x y
    -> TypeAppExt ext <$> resolveTypeShortHashes x <*> resolveTypeShortHashes y

  TypeBindingExt ext b
    -> pure $ TypeBindingExt ext b

  TypeExtensionExt ext
    -> pure $ TypeExtensionExt ext

  _ -> error "Non-exhaustive pattern in stripTypeComments"

