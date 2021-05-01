{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , LambdaCase
  , GADTs
  , ConstraintKinds
  #-}
{-|
Module      : PL.ReduceType
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Duplication of Reduce but acting at the type level. Currently has the right
to behave differently and terminate on types that could be otherwise reduced.
-}
module PL.ReduceType
  ( TypeReductionCtx (..)
  , topTypeReductionCtx

  , reduceType
  , reduceTypeStep

  , destruct
  )
  where

import PL.Bindings
import PL.Binds.Ix
import PL.Error
import PL.ExprLike
import PL.Kind
import PL.Name
import PL.Type
import PL.TyVar
import PL.TypeCtx
import PLPrinter
import PL.FixPhase

import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import qualified Data.Set as Set

-- | TypeReductionCtx contains information used/ generated when reducing an
-- expression.
data TypeReductionCtx phase = TypeReductionCtx
  { _typeReductionTypeBindings :: Bindings (TypeFor phase) -- ^ Types that are either Bound or Unbound by an outer type abstraction.
  , _typeReductionSelfType     :: Maybe (TypeFor phase)    -- ^ Assertion of the current self-type
  , _typeReductionTypeCtx      :: TypeCtxFor phase         -- ^ Associated named types to their TypeInfo definitions.
  , _typeReductionGas          :: Maybe Int                -- ^ Proportional to the amount of reduction steps allowed before reduction is aborted. All reduction steps should terminate eventually however this parameter can be used to detect bugs, I.E. diverging reduction or inefficient reduction paths.
  }

-- | Reduce a top-level type with a TypeCtx mapping type-names to type
-- definitions.
topTypeReductionCtx
  :: TypeCtxFor phase
  -> TypeReductionCtx phase
topTypeReductionCtx typeCtx = TypeReductionCtx
  { _typeReductionTypeBindings = emptyBindings
  , _typeReductionSelfType     = Nothing
  , _typeReductionTypeCtx      = typeCtx
  , _typeReductionGas          = Just 100
  }

-- | True when the ReductionCtx has been provided limited gas which has been
-- depleted.
hitTypeReductionLimit
  :: TypeReductionCtx phase
  -> Bool
hitTypeReductionLimit ctx = case _typeReductionGas ctx of
  Nothing
    -> False
  Just g
    -> g <= 0

-- | If the amount of reductions has a gas limit, reduce the amount available.
reduceTypeReductionLimit
  :: TypeReductionCtx phase
  -> TypeReductionCtx phase
reduceTypeReductionLimit ctx = ctx{_typeReductionGas = fmap (subtract 1) $ _typeReductionGas ctx}

-- | Query for the Bound/Unbound Type associated with a type variable binding.
--
-- Fails when the variable supplied is too large.
lookupTyVarBinding
  :: ( HasAbs     (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs  (TypeFor phase)

     , ErrorType    phase ~ TypeFor phase
     , ErrorTypeCtx phase ~ TypeCtxFor phase
     )
  => TypeReductionCtx phase
  -> TyVar
  -> Either (ErrorFor phase) (Binding (TypeFor phase))
lookupTyVarBinding ctx b =
  let ix       = bindDepth b
      bindings = _typeReductionTypeBindings ctx
   in case safeIndex (Proxy :: Proxy TyVar) bindings ix of
        -- We've been asked to bind a type further away than there are
        -- abstractions.
        -- This indicates either a logic error in the reduction or a failure
        -- to type-check/ validate the input AST for this case.
        --
        -- TODO:
        -- - Are invalid bindings caught at the type-checking phase?
        -- - Can we check in an earlier phase and avoid doing so here?
        Nothing
          -> Left . EBindTypeLookupFailure ix $ bindings

        Just binding
          -> Right binding

-- | Context after a Type is applied.
underTypeAppliedType
  :: TypeFor phase
  -> TypeReductionCtx phase
  -> TypeReductionCtx phase
underTypeAppliedType appliedType ctx = ctx{_typeReductionTypeBindings = bind appliedType . _typeReductionTypeBindings $ ctx}

-- | Context under some type abstraction where the type variable has _not_ been
-- bound.
underTypeTypeAbstraction
  :: TypeReductionCtx phase
  -> TypeReductionCtx phase
underTypeTypeAbstraction ctx = ctx{_typeReductionTypeBindings = unbound . bury . _typeReductionTypeBindings $ ctx}

-- | Context under the assertion that the self type is some type.
underSelfType
  :: TypeFor phase
  -> TypeReductionCtx phase
  -> TypeReductionCtx phase
underSelfType itselfTy ctx = ctx{_typeReductionSelfType = Just itselfTy}

-- | Reducing a Type means to walk down the AST that is presumed to be:
-- - Type checked
-- - Kind checked
-- - Stripped of unecessary extensions such as comments
--
-- And attempt to reduce it to it's normal form.
--
-- Reducing means to:
--
-- - Replace bound typebindings with the bound type.
-- - Leave unbound bindings intact, ensuring they are adjusted to point at the
--   correct abstraction where necessary.
--
-- - Reducing has strict semantics meaning:
--   - Arguments to type application are reduced themselves before being
--   substituted into the body of the type function.
reduceType
  :: ( Eq (TypeFor phase)
     , NoExt ~ TypeAppExtension phase
     , TyVar ~ TypeBindingFor phase
     , HasAbs (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs (TypeFor phase)

     , Ord (TypeFor phase)
     , ErrorType phase ~ TypeFor phase
     , ErrorTypeCtx phase ~ TypeCtxFor phase
     , ErrorTypeName phase ~ TypeName
     )
  => TypeReductionCtx phase
  -> TypeFor phase
  -> Either (ErrorFor phase) (TypeFor phase)
reduceType ctx t
  | hitTypeReductionLimit ctx
   = Left . ETypeReductionLimitReached $ t

  | otherwise
   = do -- Apply the reduce step until the expression no longer changes.
        -- This requires that reduction eventually converges - diverging will lead to
        -- non-termination.
        -- TODO: reduceTypeStep only reduces one level under types for each call. If
        -- we assume the type reduces, we should reduce faster by recursing with
        -- reduceTypeWith instead.
        reducedT <- reduceTypeStep ctx t
        if reducedT == t
          then pure t
          else reduceType (reduceTypeReductionLimit ctx) reducedT

-- | Constraints for functions which reduce types require that:
-- - Types are used for abstraction
-- - TyVars are used for type variables
type TypeReductionConstraints phase =
  ( NoExt ~ TypeAppExtension phase
  , TyVar ~ TypeBindingFor phase

  , HasAbs     (TypeFor phase)
  , HasBinding (TypeFor phase) TyVar
  , HasNonAbs  (TypeFor phase)

  , Ord (TypeFor phase)
  , ErrorType     phase ~ TypeFor phase
  , ErrorTypeCtx  phase ~ TypeCtxFor phase
  , ErrorTypeName phase ~ TypeName
  )

-- | 'reduceType' a single step with a collection of initial type bindings as if
-- Types have already been applied to an outer type lambda abstraction.
reduceTypeStep
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> TypeFor phase
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeStep ctx = \case
  TypeBindingExt ext b
    -> reduceTypeBinding ctx (ext, b)

  TypeSelfBindingExt ext
    -> reduceTypeSelfBinding ctx ext

  TypeContentBindingExt ext c
    -> reduceTypeContentBinding ctx (ext, c)

  NamedExt ext n
    -> reduceNamed ctx (ext, n)

  TypeAppExt ext f x
    -> reduceTypeApp ctx (ext, f, x)

  TypeLamExt ext takeKind tyBody
    -> reduceTypeLambda ctx (ext, takeKind, tyBody)

  TypeMuExt ext expectKind itselfTy
    -> reduceTypeMu ctx (ext, expectKind, itselfTy)

  BigArrowExt ext fromKind toTy
    -> reduceBigArrow ctx (ext, fromKind, toTy)

  ArrowExt ext from to
    -> reduceArrow ctx (ext, from, to)

  SumTExt ext types
    -> reduceSumT ctx (ext, types)

  ProductTExt ext types
    -> reduceProductT ctx (ext, types)

  UnionTExt ext types
    -> reduceUnionT ctx (ext, types)

  _ -> error "Non-exhaustive pattern in type reduction"

-- TypeBindings are substituted if they have been bound.
reduceTypeBinding
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (TypeBindingExtension phase, TypeBindingFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeBinding ctx (ext, b) = do
  binding <- lookupTyVarBinding ctx b
  pure $ case binding of
    -- If no type has been bound there is no further reduction
    -- possible.
    -- E.G. We're looking at '?0' in a type lambda that has not been
    -- applied.
    Unbound
      -> TypeBindingExt ext b

    -- If a type has been bound, we substitute it for the binding.
    -- We assume strict evaluation of the type and so don't reduce it
    -- again.
    Bound boundTy
      -> boundTy

  -- TODO: Should we reduce here or should reduction of selves be explicit?
reduceTypeSelfBinding
  :: TypeReductionCtx phase
  -> NoExt
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeSelfBinding _ctx ext = pure $ TypeSelfBindingExt ext

-- ContentBindings are not (currently) reduced - they're sticking points for
-- evaluation.
--
-- They _could_ be as the lack of self-reference and mutual recursion means
-- reduction would terminate.
--
-- We choose not to as:
-- - We expect them to be substituted in the evaluation phase.
-- - Leaving un-reduced allows them to be made recursive/ self-referential in
--  the future without changing behaviour here.
-- - The final result of a reduction may be stored and currently serves the
--   purpose of both the 'compiled' and human(ish) readable form. Some
--   expressions would look unrecognisable after reducing under names making
--   modification hard. It is likely we'll end up storing multiple
--   representations of the 'same' code to deal with this. For now, we
--   prioritise readability over full reduction.
reduceTypeContentBinding
  :: TypeReductionCtx phase
  -> (TypeContentBindingExtension phase, TypeContentBindingFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeContentBinding _ctx (ext, c) = pure $ TypeContentBindingExt ext c

-- Reduce using strictish semantics:
-- - First reduce a step under the applied type
-- - Then reduce a step under the type function
-- - Then bind the type and reduce the entire type a step.
--
-- Typechecking _should_ have ensured the type function is an arrow kind that
-- matches the type. It should therefore only be a type lambda or a type binding
-- to a (possibly unknown) function.
reduceTypeApp
  :: forall phase
   . TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (TypeAppExtension phase, TypeFor phase, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeApp ctx (_ext, f, x) = do
  case f of
    -- Type lambdas are reduced by binding applied values into the body
    -- and reducing.
    TypeLamExt _ext _absKind fBodyType
      -> do x' <- reduceTypeStep ctx x
            reduceTypeStep (underTypeAppliedType x' ctx) fBodyType

    -- TODO: Should we peek a level under the Mu to determine whether its a
    -- TypeLam?
    TypeMuExt ext expectKind itselfTy
      -> reduceTypeMu (underSelfType itselfTy ctx) (ext, expectKind, itselfTy)

    -- The function we're applying is 'higher order' - it is sourced
    -- from a function itself.
    --
    -- If a type was bound it should have been substituted in the
    -- function reduction and so we can assume it is unbound and do
    -- nothing except reduce the argument a step.
    -- If we're wrong an additional reduceStep on the
    -- application should make progress.
    TypeBindingExt ext tyVar
      -> TypeApp <$> reduceTypeBinding ctx (ext, tyVar)
                 <*> reduceTypeStep ctx x

    -- We don't reduce under content bindings.
    TypeContentBindingExt ext c
      -> TypeApp <$> pure (TypeContentBindingExt ext c)
                 <*> reduceTypeStep ctx x

    TypeSelfBindingExt _ext
      -> case _typeReductionSelfType ctx of
           Nothing
             -> Left . EMsg . text $ "Cant reduce type application with a self-type which does not exist"

           Just selfTy
             -> TypeApp <$> pure selfTy <*> reduceTypeStep ctx x

    -- We're applying a named type which should be looked up in the type
    -- context.
    --
    -- Similar to TypeBindings - this should have been reduced away
    -- already.
    NamedExt ext n
      -> TypeApp
           <$> reduceNamed ctx (ext, n)
           <*> reduceTypeStep ctx x

    -- A nested type application in function position.
    TypeAppExt nestedExt nestedF nestedX
      -> TypeAppExt nestedExt
           <$> reduceTypeApp ctx (nestedExt, nestedF, nestedX)
           <*> pure nestedX

    -- An error here indicates type/kind checking has not been performed/ has
    -- been performed incorrectly as the type in type function
    -- position is not a lambda.
    _ -> Left $ ETypeAppLambda f

-- Reduce a single step under the type lambda type.
reduceTypeLambda
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (TypeLamExtension phase, Kind, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeLambda ctx (ext, takeKind, tyBody) = (TypeLamExt ext takeKind) <$> reduceTypeStep (underTypeTypeAbstraction ctx) tyBody

reduceTypeMu
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (NoExt, Kind, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceTypeMu _ctx (ext, expectKind, itselfTy) = pure $ TypeMuExt ext expectKind itselfTy
-- TODO: If self-types had a bindctx they could be unbound, allowing us to
-- reduce the definition.

-- Reduce a single step under the big arrow types.
reduceBigArrow
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (BigArrowExtension phase, Kind, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceBigArrow ctx (ext, fromKind, toTy) = BigArrowExt ext fromKind <$> reduceTypeStep ctx toTy
-- TODO: Should bindings be wrapped with 'unbound'?

-- Return the non-Mu definition of a name if it is known.
--
-- If the definition is a Mu type, the original name is returned. To get a
-- recursive definition, careful use of lookupTypeNameInitialType and destruct
-- is required.
reduceNamed
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (NamedExtension phase, TypeName)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceNamed ctx (ext, n) =
  case lookupTypeNameInitialType n (_typeReductionTypeCtx ctx) of
    Nothing
      -> Left $ ETypeNotDefined n (_typeReductionTypeCtx ctx)

    -- Do not expose the definition of recursive types so that recursive
    -- reduction terminates!
    -- TODO: Mu types that don't contain a self-reference _could_ be returned.
    -- Arguably they are useless and shouldnt have passed the type-checker in
    -- the first place.
    Just (TypeMuExt _ _ _)
      -> pure $ NamedExt ext n

    Just t
      -> Right t

-- Reduce a single step under the arrow type.
reduceArrow
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (ArrowExtension phase, TypeFor phase, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
reduceArrow ctx (ext, from, to) = ArrowExt ext <$> reduceTypeStep ctx from
                                               <*> reduceTypeStep ctx to

-- Reduce a single step under the sum types.
reduceSumT
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (SumTExtension phase, NonEmpty (TypeFor phase))
  -> Either (ErrorFor phase) (TypeFor phase)
reduceSumT ctx (ext, types) = SumTExt ext <$> mapM (reduceTypeStep ctx) types

-- Reduce a single step under the product types.
reduceProductT
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (ProductTExtension phase, [TypeFor phase])
  -> Either (ErrorFor phase) (TypeFor phase)
reduceProductT ctx (ext, types) = ProductTExt ext <$> mapM (reduceTypeStep ctx) types

-- Reduce a single step under the union types.
reduceUnionT
  :: TypeReductionConstraints phase
  => TypeReductionCtx phase
  -> (UnionTExtension phase, Set.Set (TypeFor phase))
  -> Either (ErrorFor phase) (TypeFor phase)
reduceUnionT ctx (ext, types) = (UnionTExt ext . Set.fromList) <$> mapM (reduceTypeStep ctx) (Set.toList types)

-- | Replace a single layer of self-references within a Mu type.
--
-- - The input Mu type is deconstructed into a triple of its extension, its kind
--   and its itself-definition.
-- - There is no substitution under binders like names, type bindings or content
--   bindings as self-bindings are not allowed at the top level and we dont
--   (currently) support nested self-types.
destruct
  :: forall phase
   . Ord (TypeFor phase)
  => (NoExt, Kind, TypeFor phase)
  -> Either (ErrorFor phase) (TypeFor phase)
destruct (muExt, muKind, itselfT) = destruct' itselfT
  where
    destruct' :: TypeFor phase -> Either (ErrorFor phase) (TypeFor phase)
    destruct' = \case
      -- Found a self-binding. Substitute.
      TypeSelfBindingExt _
        -> Right $ TypeMuExt muExt muKind itselfT

      -- Self bindings cannot reference further than their nearest Mu and so
      -- theres no need to recurse on the following constructors.
      -- If self bindings are extended, we should recurse here:

      TypeMuExt _ _ _
        -> pure itselfT
      -- Bindings should not contain bare self-bindings.
      TypeBindingExt _ _
        -> pure itselfT
      -- ContentBindings should not contain bare self-bindings.
      TypeContentBindingExt _ _
        -> pure itselfT
      -- Names should not contain bare self-bindings.
      NamedExt ext n
        -> pure $ NamedExt ext n

      -- Constructors to recurse upon:

      TypeAppExt ext f x
        -> TypeAppExt ext
             <$> destruct' f
             <*> destruct' x

      TypeLamExt ext takeKind tBody
        -> TypeLamExt ext takeKind
             <$> destruct' tBody

      BigArrowExt ext fromKind toTy
        -> BigArrowExt ext fromKind
            <$> destruct' toTy

      ArrowExt ext from to
        -> ArrowExt ext
             <$> destruct' from
             <*> destruct' to

      SumTExt ext types
        -> SumTExt ext
             <$> mapM destruct' types

      ProductTExt ext types
        -> ProductTExt ext
             <$> mapM destruct' types

      UnionTExt ext types
        -> (UnionTExt ext . Set.fromList)
             <$> mapM destruct' (Set.toList types)

      _ -> error "Non-exhaustive pattern in destruct type mu"

