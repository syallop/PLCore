{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , GADTs
  , ConstraintKinds
  #-}
module PL.Type.Eq
  ( typeEq
  , typeEqs
  , typeKind
  )
  where

import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Error
import PL.Name
import PL.ExprLike
import PL.Kind
import PL.TyVar
import PLPrinter
import PL.ReduceType
import PL.Type
import PL.TypeCtx
import PL.FixPhase

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Debug.Trace

-- TODO: Consider adding/ sharing a Ctx type to thread through these functions.

-- | Test whether two types are equal under some context and with a small amount
-- of gas used to limit the amount of reduction steps. 'typeEqWith' accepts
-- dn explicit quantity.
typeEq
  :: ( TyVar ~ TypeBindingFor phase
     , Void  ~ TypeAppExtension phase

     , Show (TypeFor phase)
     , Ord (TypeFor phase)

     , HasAbs (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs (TypeFor phase)
     )
  => BindCtx TyVar Kind   -- ^ Associate type variables to their kinds
  -> Bindings (TypeFor phase) -- ^ Type variables are either unbound or bound with some type
  -> TypeCtxFor phase              -- ^ Associate named types to their TypeInfo definition
  -> Map ContentName (TypeFor phase) -- ^ Cache a mapping of known type content names to their type definitions
  -> TypeFor phase
  -> TypeFor phase
  -> Either (Error expr (TypeFor phase) patternFor (TypeCtxFor phase)) Bool
typeEq typeBindCtx typeBindings typeNameCtx contentIsType type0 type1 = typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (Just 128) type0 type1

typeEqWith
  :: ( TyVar ~ TypeBindingFor   phase
     , Void  ~ TypeAppExtension phase

     , Show (TypeFor phase)
     , Ord (TypeFor phase)

     , HasAbs (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs (TypeFor phase)
     )
  => BindCtx TyVar Kind
  -> Bindings (TypeFor phase)
  -> TypeCtxFor phase
  -> Map ContentName (TypeFor phase)
  -> Maybe Int
  -> TypeFor phase
  -> TypeFor phase
  -> Either (Error expr (TypeFor phase) patternFor (TypeCtxFor phase)) Bool
typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit type0 type1
  | reductionLimit == Just 0
   = Left . EMsg . mconcat $
       [ text "Reduction limit reached when type-checking expression. Comparing two types:"
       , lineBreak
       , indent1 . text . Text.pack . show $ type0
       , lineBreak
       , text "And:"
       , lineBreak
       , indent1 . text . Text.pack . show $ type1
       ]

  | otherwise
   = case (type0, type1) of
       -- Named Types are ONLY equal if they have the same name.
       -- TODO: Should we require the name exists in the context as well?
       (NamedExt _ n0, NamedExt _ n1)
         | n0 == n1  -> Right True

         -- TODO: We may want different names for the same definition to be
         -- equal.
         --
         -- This currently doesn't appear to be a global problem due to how callers
         -- take care to reduce types to their definition before considering
         -- calling typeEq.
         | otherwise -> Right False

       -- To compare a Named type to a non-named type, lookup the definition of the
       -- name and recurse.
       (_, NamedExt _ _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit type1 type0
       (NamedExt _ n0, _)
         -> let it0 = lookupTypeNameInitialInfo n0 typeNameCtx
             in case it0 of
                  Nothing
                    -> Left $ EMsg $ text "Failed to lookup named type"

                  Just type0
                    -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) (_typeInfoType type0) type1

       -- ContentBindings are equal if they have the same hash.
       -- TODO: Should we require the name exists in the context as well?
       (TypeContentBindingExt _ c0, TypeContentBindingExt _ c1)
         | c0 == c1  -> Right True
         | otherwise -> Right False

       -- Compare a content-binding to a non-content binding.
       (type0, TypeContentBindingExt _ _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit type1 type0
       (TypeContentBindingExt _ c0, type1)
         -> case Map.lookup c0 contentIsType of
              Nothing
                -> Left . EMsg . mconcat $
                     [ text "Cannot check equality of unknown TypeContentBinding:"
                     , string . show $ c0
                     ]

              Just resolvedType0
                -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) resolvedType0 type1

       -- Type bindings are 'equal' when they unify.
       -- TODO: If we're unifying unbound types with types, should we be back
       -- propogating this unification? If so, the current data structures and
       -- algorithm is not appropriate.
       (TypeBindingExt _ b0, TypeBindingExt _ b1)
         -> do binding0 <- maybe (Left $ EBindTypeLookupFailure (bindDepth b0) typeBindings) Right $ safeIndex (Proxy :: Proxy TyVar) typeBindings (bindDepth b0)
               binding1 <- maybe (Left $ EBindTypeLookupFailure (bindDepth b1) typeBindings) Right $ safeIndex (Proxy :: Proxy TyVar) typeBindings (bindDepth b1)
               case (binding0,binding1) of
                  -- Two unbound are unified
                  (Unbound, Unbound)
                    -> Right True

                  -- An unbound unifies with a bound
                  (Unbound, Bound ty1)
                    -> Right True

                  (Bound ty1, Unbound)
                    -> Right True

                  -- Two bound types are equal if the bound types are equal
                  (Bound type0, Bound type1)
                    -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) type0 type1

       -- To compare a binding to a non-binding
       (type0, TypeBindingExt _ _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit type1 type0
       (TypeBindingExt _ b0, type1)
         -> let binding0 = index (Proxy :: Proxy TyVar) typeBindings (bindDepth b0)
               in case binding0 of
                    Unbound
                      -> Right True

                    Bound type0
                      -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) type0 type1

       -- Two type applications are equal if both of their corresponding parts are
       -- equal
       (TypeAppExt _ f0 x0, TypeAppExt _ f1 x1)
         -> (&&) <$> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) f0 f1
                 <*> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) x0 x1

       -- A TypeApp is equal to something else when, after reducing it, it is equal
       -- to that other thing.
       (ty0, TypeAppExt _ _ _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit type1 ty0
       (TypeAppExt _ f0 x0, ty1)
         -> case reduceTypeStep (TypeReductionCtx typeBindings typeNameCtx (Just 100)) f0 of
              Left e
                -> Left e

              Right redF0
                -> case redF0 of
                     -- TODO: we're assuming kindchecking has already been performed. Otherwise, aKy must equal xKy
                     -- The resulting type is then the rhs of the typelam under the
                     -- context of what it was applied to
                     TypeLamExt _ aKy bTy
                       -> typeEqWith (addBinding aKy typeBindCtx) (bind x0 typeBindings) typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) bTy ty1

                     _ -> Left $ EMsg $ text "In typeEq: Cant type apply a non-lambda type to a type"


       -- Arrow types are equal when their corresponding to and from types are the
       -- same
       (ArrowExt _ from0 to0, ArrowExt _ from1 to1)
         -> (&&) <$> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) from0 from1
                 <*> typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) to0   to1

       (SumTExt _ ts0, SumTExt _ ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) (NE.toList ts0) (NE.toList ts1)

       (ProductTExt _ ts0, ProductTExt _ ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) ts0 ts1

       (UnionTExt _ ts0, UnionTExt _ ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) (Set.toList ts0) (Set.toList ts1)

       (BigArrowExt _ fromKy0 toTy0, BigArrowExt _ fromKy1 toTy1)
         -> let newTypeBindCtx  = addBinding fromKy1 typeBindCtx
                newTypeBindings = unbound $ typeBindings
               in if kindEq fromKy0 fromKy1
                    then typeEqWith newTypeBindCtx newTypeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) toTy0 toTy1
                    else Right False

       -- Two type lambda are equivalent if their corresponding from and to are the
       -- same
       -- TODO: Should probably UnBound the type vars when checking theyre equal
       (TypeLamExt _ k0 ty0, TypeLamExt _ k1 ty1)
         -> let newTypeBindCtx  = addBinding k0 typeBindCtx
                newTypeBindings = unbound $ typeBindings
               in (&&) <$> pure (k0 == k1)
                       <*> typeEqWith newTypeBindCtx newTypeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit) ty0 ty1

       -- A non-Named, non identical type
       _ -> Right False

-- Are two lists of types pairwise equivalent under a typectx?
typeEqs
  :: ( TyVar ~ TypeBindingFor phase
     , Void ~ TypeAppExtension phase

     , Show (TypeFor phase)
     , Ord (TypeFor phase)

     , HasAbs (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs (TypeFor phase)
     )
  => BindCtx TyVar Kind
  -> Bindings (TypeFor phase)
  -> TypeCtxFor phase
  -> Map ContentName (TypeFor phase)
  -> [TypeFor phase]
  -> [TypeFor phase]
  -> Either (Error expr (TypeFor phase) pattern (TypeCtxFor phase)) Bool
typeEqs typeBindCtx typeBindings typeNameCtx contentIsType ts0 ts1 = typeEqsWith typeBindCtx typeBindings typeNameCtx contentIsType (Just 128) ts0 ts1

-- Are two lists of types pairwise equivalent under a typectx?
typeEqsWith
  :: ( TyVar ~ TypeBindingFor phase
     , Void ~ TypeAppExtension phase
     , Show (TypeFor phase)
     , Ord (TypeFor phase)

     , HasAbs (TypeFor phase)
     , HasBinding (TypeFor phase) TyVar
     , HasNonAbs (TypeFor phase)
     )
  => BindCtx TyVar Kind
  -> Bindings (TypeFor phase)
  -> TypeCtxFor phase
  -> Map ContentName (TypeFor phase)
  -> Maybe Int
  -> [TypeFor phase]
  -> [TypeFor phase]
  -> Either (Error expr (TypeFor phase) pattern (TypeCtxFor phase)) Bool
typeEqsWith typeBindCtx typeBindings typeNameCtx contentIsType reductionLimit ts0 ts1
  | reductionLimit == Just 0
   = Left . EMsg . mconcat $
       [ text "Reduction limit reached when type-checking two lists of expressions:"
       , lineBreak
       , indent1 . text . Text.pack . show $ ts0
       , lineBreak
       , text "And:"
       , lineBreak
       , indent1 . text . Text.pack . show $ ts1
       ]

  | length ts0 == length ts1 = let mEq = and <$> zipWithM (typeEqWith typeBindCtx typeBindings typeNameCtx contentIsType (fmap (subtract 1) reductionLimit)) ts0 ts1
                                  in case mEq of
                                       Right True -> mEq
                                       _          -> mEq
  | otherwise                = Right False

-- | Under a given bindings context, kind-check a type.
-- TODO: Break move to more appropriate location.
typeKind
  :: ( TyVar ~ TypeBindingFor phase
     )
  => BindCtx TyVar Kind
  -> Map ContentName Kind
  -> TypeCtxFor phase
  -> TypeFor phase
  -> Either (Error expr (TypeFor phase) pattern (TypeCtxFor phase)) Kind
typeKind typeBindCtx contentKinds typeCtx ty = case ty of

  -- TODO: Probably wack. The definition can refer to itself if it is recursive, which will send the kind checker into an infinite loop.
  -- Either:
  -- - Tag types with their kind when theyre added to the context, and trust that.
  -- - ???
  NamedExt _ n
    -> case lookupTypeNameInitialInfo n typeCtx of
         Nothing
           -> Left $ EMsg $ text "No such type name in kind check"

         Just (TypeInfo _ ky _)
           -> Right ky

  --
  --   from :: fromKy     to :: toKy
  -- ----------------------------------
  --       Arrow from to :: Kind
  ArrowExt _ from to
    -> do -- both from and to must kind-check
          _ <- typeKind typeBindCtx contentKinds typeCtx from
          _ <- typeKind typeBindCtx contentKinds typeCtx to

          Right Kind

  --
  --
  SumTExt _ types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx contentKinds typeCtx) types
          Right Kind

  --
  --
  ProductTExt _ types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx contentKinds typeCtx) types
          Right Kind

  --
  --
  UnionTExt _ types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx contentKinds typeCtx) (Set.toList types)
          Right Kind

  --
  --
  BigArrowExt _ fromKy toTy
    -> do let newTypeBindCtx = addBinding fromKy typeBindCtx
          -- TODO: should this be KindArrow?? like TypeLam.
          -- Is that what makes them different or should they be the same thing???
          _ <- typeKind newTypeBindCtx contentKinds typeCtx toTy
          Right Kind
  --
  --
  TypeBindingExt _ b
    -> case lookupBindingTy b typeBindCtx of
         Nothing
           -> Left $ EContext (EMsg $ text "Kind-checking type") $ EBindCtxTypeLookupFailure (fromEnum b) typeBindCtx
         Just ky
           -> Right ky

  --
  --
  TypeContentBindingExt _ contentName
    -> case Map.lookup contentName contentKinds of
         Nothing
           -> Left . EMsg . mconcat $
                [ text "Could not find a kind association for content named: "
                , lineBreak
                , indent1 . string . show $ contentName
                , lineBreak
                ]

         Just k
           -> Right k

  --
  --
  TypeLamExt _ absKy ty
    -> do -- type must kind-check under the introduction of a new abstraction to the typeBindCtx
          let newTypeBindCtx = addBinding absKy typeBindCtx
          tyKy <- typeKind newTypeBindCtx contentKinds typeCtx ty
          Right $ KindArrow absKy tyKy

  --
  --
  TypeAppExt _ fTy xTy
    -> do -- both f and x must kind-check
          fKy <- typeKind typeBindCtx contentKinds typeCtx fTy
          xKy <- typeKind typeBindCtx contentKinds typeCtx xTy

          -- resolve the initial kind here if named kinds are added..

          case fKy of
            -- Plain Kinds cannot be applied
            Kind
              -> Left $ ETypeAppMismatch fKy xKy

            -- Regular big application attempt
            KindArrow aKy bKy

              -- The kind of the provided xTy, and the kind expected by the left of fTy's kind arrow
              -- match => The kind is the right hand side of the kind arrow
              | kindEq aKy xKy -> Right bKy
              | otherwise      -> Left $ ETypeAppMismatch fKy xKy

  _ -> error "Non-exhaustive pattern when checking types kind"

