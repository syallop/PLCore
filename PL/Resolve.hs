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

This module defines a compiler phase which can be used to:
- Resolve ShortHashes within {expr,type,pattern}s to unambiguous Hashes
- Shorten Hashes within {expr,type,pattern}s to the shortest -currently unambiguous- ShortHash

Resolution would typically take place after parsing and before type-checking.
Shortening would typically take place before printing.

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

  -- Shorten Hashes within {expr,type,pattern}s to the smallest - currently -
  -- unambiguous ShortHash.
  , shortenHashes
  , shortenTypeHashes
  , shortenPatternHashes

  , shortenCaseHashes
  , shortenCaseBranchesHashes
  , shortenCaseBranchHashes

  -- Shorten individual hashes to the smallest unambiguous hash
  , shortenExprHash
  , shortenTypeHash
  , shortenKindHash

  -- Claim that two phases are identical with one using ShortHashes and the
  -- other resolved Hashes.
  , ExprWithResolvedHashes
  , TypeWithResolvedHashes
  , PatternWithResolvedHashes

  -- Inverse of *WithResolvedHashes
  , ExprWithShortenedHashes
  , TypeWithShortenedHashes
  , PatternWithShortenedHashes
  )
  where

import qualified PL.CodeStore as C
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
  { _resolveCodeStore :: C.CodeStore -- ^ Backing CodeStore consulted to map ShortHashes to Hashes.
  }

-- | Create a ResolveCtx with a CodeStore that can be consulted as the authority
-- on whether ShortHashes are prefixes of any larger Hashes.
mkResolveCtx
  :: C.CodeStore
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
  mRes <- C.largerExprHashes (_resolveCodeStore ctx) shortHash
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
  mRes <- C.largerTypeHashes (_resolveCodeStore ctx) shortHash
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
  mRes <- C.largerKindHashes (_resolveCodeStore ctx) shortHash
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

-- | Given an Expr Hash, shorten it to the shortest unambiguous ShortHash.
shortenExprHash
  :: Hash
  -> Resolve ShortHash
shortenExprHash exprHash = Resolve $ \ctx -> do
  mRes <- C.shortenExprHash (_resolveCodeStore ctx) exprHash
  case mRes of
    Nothing
      -> pure . Left . EContext (EMsg . mconcat $ [ text "Shortening expr hash:"
                                                  , lineBreak
                                                  , indent1 . string . show $ exprHash
                                                  ]
                                ) $ EMsg $ text "Unknown error in underlying code store"

    Just (codeStore', shortHash)
      -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, shortHash)

-- | Given a Type Hash, shorten it to the shortest unambiguous ShortHash.
shortenTypeHash
  :: Hash
  -> Resolve ShortHash
shortenTypeHash typeHash = Resolve $ \ctx -> do
  mRes <- C.shortenTypeHash (_resolveCodeStore ctx) typeHash
  case mRes of
    Nothing
      -> pure . Left . EContext (EMsg . mconcat $ [ text "Shortening type hash:"
                                                  , lineBreak
                                                  , indent1 . string . show $ typeHash
                                                  ]
                                ) $ EMsg $ text "Unknown error in underlying code store"

    Just (codeStore', shortHash)
      -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, shortHash)

-- | Given a Kind Hash, shorten it to the shortest unambiguous ShortHash.
shortenKindHash
  :: Hash
  -> Resolve ShortHash
shortenKindHash kindHash = Resolve $ \ctx -> do
  mRes <- C.shortenKindHash (_resolveCodeStore ctx) kindHash
  case mRes of
    Nothing
      -> pure . Left . EContext (EMsg . mconcat $ [ text "Shortening kind hash:"
                                                  , lineBreak
                                                  , indent1 . string . show $ kindHash
                                                  ]
                                ) $ EMsg $ text "Unknown error in underlying code store"

    Just (codeStore', shortHash)
      -> pure . Right $ (ctx{_resolveCodeStore=codeStore'}, shortHash)


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

-- | Two expression phases are identical but:
-- - The first uses unambiguous Hashes
-- - The second uses shortened - potentially ambiguous - ShortHashes
type ExprWithShortenedHashes long short = ExprWithResolvedHashes short long

-- | Two type phases are identical but:
-- - The first uses potentially ambiguous ShortHashes
-- - The second uses unambiguous Hashes
type TypeWithResolvedHashes unresolved resolved =
  ( TypeContentBindingFor unresolved ~ ShortHash
  , TypeContentBindingFor resolved   ~ ContentName

  , Ord (TypeFor resolved)
  , Ord (TypeFor unresolved)

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

-- | Two type phases are identical but:
-- - The first uses unambiguous Hashes
-- - The second uses shortened - potentially ambiguous - ShortHashes
type TypeWithShortenedHashes long short = TypeWithResolvedHashes short long

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

-- | Two pattern phases are identical but:
-- - The first uses unambiguous Hashes
-- - The second uses shortened - potentially ambiguous - ShortHashes
type PatternWithShortenedHashes long short = PatternWithResolvedHashes short long

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

-- | Attempt to shorten all Hashes contained within an expression (it's patterns
-- and it's types) into the shortest unambiguous ShortHashes.
shortenHashes
  :: ( ExprWithShortenedHashes long short
     , AbstractionFor long ~ TypeFor typePhase
     , AbstractionFor short ~ TypeFor typePhase
     )
  => ExprFor long
  -> Resolve (ExprFor short)
shortenHashes e = case e of
  ContentBindingExt ext c
    -> ContentBindingExt ext <$> (shortenExprHash . contentName $ c)

  LamExt ext abs bodyExpr
    -> LamExt ext abs <$> shortenHashes bodyExpr

  AppExt ext fExpr xExpr
    -> AppExt ext <$> shortenHashes fExpr <*> shortenHashes xExpr

  CaseAnalysisExt ext c
    -> CaseAnalysisExt ext <$> shortenCaseHashes c

  SumExt ext expr ix ty
    -> SumExt ext <$> shortenHashes expr <*> pure ix <*> mapM shortenTypeHashes ty

  ProductExt ext prodExprs
    -> ProductExt ext <$> mapM shortenHashes prodExprs

  UnionExt ext unionExpr tyIx ty
    -> UnionExt ext <$> shortenHashes unionExpr
                    <*> shortenTypeHashes tyIx
                    <*> (fmap Set.fromList $ mapM shortenTypeHashes $ Set.toList ty)

  BindingExt ext b
    -> pure $ BindingExt ext b

  BigLamExt ext takeTy expr
    -> BigLamExt ext takeTy <$> shortenHashes expr

  BigAppExt ext fExpr xTy
    -> BigAppExt ext <$> shortenHashes fExpr <*> shortenTypeHashes xTy

  ExprExtensionExt ext
    -> pure $ ExprExtensionExt ext

  _ -> error "Non-exhaustive pattern shortening hashes in expression"

-- | Attempt to shorten all Hashes contained within a case expression
-- to the shortest unambiguous ShortHashes.
shortenCaseHashes
  :: ( ExprWithShortenedHashes long short
     , AbstractionFor long ~ TypeFor typePhase
     , AbstractionFor short ~ TypeFor typePhase
     )
  => Case (ExprFor long) (PatternFor long)
  -> Resolve (Case (ExprFor short) (PatternFor short))
shortenCaseHashes (Case expr branches) = Case <$> shortenHashes expr <*> shortenCaseBranchesHashes branches

-- | Attempt to shorten all Hashes contained within case branches
-- to the shortest unambiguous ShortHashes.
shortenCaseBranchesHashes
  :: ( ExprWithShortenedHashes long short
     , AbstractionFor long ~ TypeFor typePhase
     , AbstractionFor short ~ TypeFor typePhase
     )
  => CaseBranches (ExprFor long) (PatternFor long)
  -> Resolve (CaseBranches (ExprFor short) (PatternFor short))
shortenCaseBranchesHashes = \case
  DefaultOnly expr
    -> DefaultOnly <$> shortenHashes expr

  CaseBranches neBranches mDefault
    -> CaseBranches <$> mapM shortenCaseBranchHashes neBranches
                    <*> mapM shortenHashes mDefault

-- | Attempt to shorten all Hashes contained within a case branch
-- to the shortest unambiguous ShortHashes.
shortenCaseBranchHashes
  :: ( ExprWithShortenedHashes long short
     , AbstractionFor long ~ TypeFor typePhase
     , AbstractionFor short ~ TypeFor typePhase
     )
  => CaseBranch (ExprFor long) (PatternFor long)
  -> Resolve (CaseBranch (ExprFor short) (PatternFor short))
shortenCaseBranchHashes (CaseBranch match result) = CaseBranch <$> shortenPatternHashes match <*> shortenHashes result

-- | Attempt to shorten all Hashes contained within a pattern
-- to the shortest unambiguous ShortHashes.
shortenPatternHashes
  :: PatternWithShortenedHashes long short
  => PatternFor long
  -> Resolve (PatternFor short)
shortenPatternHashes = \case
  BindExt ext
    -> pure $ BindExt ext

  BindingPatternExt ext b
    -> pure $ BindingPatternExt ext b

  SumPatternExt ext sumIndex nestedPattern
    -> SumPatternExt ext sumIndex <$> shortenPatternHashes nestedPattern

  ProductPatternExt ext nestedPatterns
    -> ProductPatternExt ext <$> mapM shortenPatternHashes nestedPatterns

  UnionPatternExt ext  unionIndexTy nestedPattern
    -> UnionPatternExt ext <$> shortenTypeHashes unionIndexTy <*> shortenPatternHashes nestedPattern

  PatternExtensionExt ext
    -> pure $ PatternExtensionExt ext

  _ -> error "Non-exhaustive pattern match when shortening hashes in pattern"

-- | Attempt to shorten all Hashes contained within a type
-- to the shortest unambiguous ShortHashes.
shortenTypeHashes
  :: ( TypeWithShortenedHashes long short
     )
  => TypeFor long
  -> Resolve (TypeFor short)
shortenTypeHashes = \case
  TypeContentBindingExt ext c
    -> TypeContentBindingExt ext <$> (shortenTypeHash . contentName $ c)

  NamedExt ext tyName
    -> pure $ NamedExt ext tyName

  ArrowExt ext from to
    -> ArrowExt ext <$> shortenTypeHashes from <*> shortenTypeHashes to

  SumTExt ext types
    -> SumTExt ext <$> mapM shortenTypeHashes types

  ProductTExt ext types
    -> ProductTExt ext <$> mapM shortenTypeHashes types

  UnionTExt ext types
    -> UnionTExt ext <$> (fmap Set.fromList . mapM shortenTypeHashes . Set.toList $ types)

  BigArrowExt ext from to
    -> BigArrowExt ext from <$> shortenTypeHashes to

  TypeLamExt ext kind typ
    -> TypeLamExt ext kind <$> shortenTypeHashes typ

  TypeAppExt ext x y
    -> TypeAppExt ext <$> shortenTypeHashes x <*> shortenTypeHashes y

  TypeBindingExt ext b
    -> pure $ TypeBindingExt ext b

  TypeExtensionExt ext
    -> pure $ TypeExtensionExt ext

  _ -> error "Non-exhaustive pattern shortening hashes in type"

