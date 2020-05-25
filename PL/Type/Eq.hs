{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , GADTs
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
import PL.ExprLike
import PL.Kind
import PL.TyVar
import PLPrinter
import PL.ReduceType
import PL.Type
import PL.TypeCtx

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as Text

import Debug.Trace

-- TODO: Consider adding/ sharing a Ctx type to thread through these functions.

-- Are two types equivalent under a typectx?
-- TODO: Should check equality under a BindCtx tb Kind
typeEq
  :: BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx
  -> Type
  -> Type
  -> Either (Error expr Type patternFor TypeCtx) Bool
typeEq typeBindCtx typeBindings typeNameCtx type0 type1 = typeEqWith typeBindCtx typeBindings typeNameCtx (Just 100) type0 type1

typeEqWith
  :: BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx
  -> Maybe Int
  -> Type
  -> Type
  -> Either (Error expr Type patternFor TypeCtx) Bool
typeEqWith typeBindCtx typeBindings typeNameCtx reductionLimit type0 type1
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
       (Named n0, Named n1)
         | n0 == n1  -> Right True
         | otherwise -> Right False

       -- To compare a Named type to a non-named type, lookup the definition of the
       -- name and recurse.
       (_, Named _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx reductionLimit type1 type0
       (Named n0, _)
         -> let it0 = lookupTypeNameInitialInfo n0 typeNameCtx
             in case it0 of
                  Nothing
                    -> Left $ EMsg $ text "Failed to lookup named type"

                  Just type0
                    -> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) (_typeInfoType type0) type1

       -- Type bindings are 'equal' when they unify.
       -- TODO: If we're unifying unbound types with types, should we be back
       -- propogating this unification? If so, the current data structures and
       -- algorithm is not appropriate.
       (TypeBinding b0, TypeBinding b1)
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
                    -> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) type0 type1

       -- To compare a binding to a non-binding
       (type0, TypeBinding _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx reductionLimit type1 type0
       (TypeBinding b0, type1)
         -> let binding0 = index (Proxy :: Proxy TyVar) typeBindings (bindDepth b0)
               in case binding0 of
                    Unbound
                      -> Right True

                    Bound type0
                      -> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) type0 type1

       -- Two type applications are equal if both of their corresponding parts are
       -- equal
       (TypeApp f0 x0, TypeApp f1 x1)
         -> (&&) <$> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) f0 f1
                 <*> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) x0 x1

       -- A TypeApp is equal to something else when, after reducing it, it is equal
       -- to that other thing.
       (ty0, TypeApp _ _)
         -> typeEqWith typeBindCtx typeBindings typeNameCtx reductionLimit type1 ty0
       (TypeApp f0 x0, ty1)
         -> case reduceTypeStep (TypeReductionCtx typeBindings typeNameCtx (Just 100)) f0 of
              Left e
                -> Left e

              Right redF0
                -> case redF0 of
                     -- TODO: we're assuming kindchecking has already been performed. Otherwise, aKy must equal xKy
                     -- The resulting type is then the rhs of the typelam under the
                     -- context of what it was applied to
                     TypeLam aKy bTy
                       -> typeEqWith (addBinding aKy typeBindCtx) (bind x0 typeBindings) typeNameCtx (fmap (subtract 1) reductionLimit) bTy ty1

                     _ -> Left $ EMsg $ text "In typeEq: Cant type apply a non-lambda type to a type"


       -- Arrow types are equal when their corresponding to and from types are the
       -- same
       (Arrow from0 to0, Arrow from1 to1)
         -> (&&) <$> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) from0 from1
                 <*> typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) to0   to1

       (SumT ts0, SumT ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) (NE.toList ts0) (NE.toList ts1)

       (ProductT ts0, ProductT ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) ts0 ts1

       (UnionT ts0, UnionT ts1)
         -> typeEqsWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit) (Set.toList ts0) (Set.toList ts1)

       (BigArrow fromKy0 toTy0, BigArrow fromKy1 toTy1)
         -> let newTypeBindCtx  = addBinding fromKy1 typeBindCtx
                newTypeBindings = unbound $ typeBindings
               in if kindEq fromKy0 fromKy1
                    then typeEqWith newTypeBindCtx newTypeBindings typeNameCtx (fmap (subtract 1) reductionLimit) toTy0 toTy1
                    else Right False

       -- Two type lambda are equivalent if their corresponding from and to are the
       -- same
       -- TODO: Should probably UnBound the type vars when checking theyre equal
       (TypeLam k0 ty0, TypeLam k1 ty1)
         -> let newTypeBindCtx  = addBinding k0 typeBindCtx
                newTypeBindings = unbound $ typeBindings
               in (&&) <$> pure (k0 == k1)
                       <*> typeEqWith newTypeBindCtx newTypeBindings typeNameCtx (fmap (subtract 1) reductionLimit) ty0 ty1

       -- A non-Named, non identical type
       _ -> Right False

-- Are two lists of types pairwise equivalent under a typectx?
typeEqs
  :: BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx
  -> [Type]
  -> [Type]
  -> Either (Error expr Type pattern TypeCtx) Bool
typeEqs typeBindCtx typeBindings typeNameCtx ts0 ts1 = typeEqsWith typeBindCtx typeBindings typeNameCtx (Just 100) ts0 ts1

-- Are two lists of types pairwise equivalent under a typectx?
typeEqsWith
  :: BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx
  -> Maybe Int
  -> [Type]
  -> [Type]
  -> Either (Error expr Type pattern TypeCtx) Bool
typeEqsWith typeBindCtx typeBindings typeNameCtx reductionLimit ts0 ts1
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

  | length ts0 == length ts1 = let mEq = and <$> zipWithM (typeEqWith typeBindCtx typeBindings typeNameCtx (fmap (subtract 1) reductionLimit)) ts0 ts1
                                  in case mEq of
                                       Right True -> mEq
                                       _          -> mEq
  | otherwise                = Right False

-- | Under a given bindings context, kind-check a type.
-- TODO: Break move to more appropriate location.
typeKind
  :: BindCtx TyVar Kind
  -> TypeCtx
  -> Type
  -> Either (Error expr Type pattern TypeCtx) Kind
typeKind typeBindCtx typeCtx ty = case ty of

  -- TODO: Probably wack. The definition can refer to itself if it is recursive, which will send the kind checker into an infinite loop.
  -- Either:
  -- - Tag types with their kind when theyre added to the context, and trust that.
  -- - ???
  Named n
    -> case lookupTypeNameInitialInfo n typeCtx of
         Nothing
           -> Left $ EMsg $ text "No such type name in kind check"

         Just (TypeInfo _ ky _)
           -> Right ky

  --
  --   from :: fromKy     to :: toKy
  -- ----------------------------------
  --       Arrow from to :: Kind
  Arrow from to
    -> do -- both from and to must kind-check
          _ <- typeKind typeBindCtx typeCtx from
          _ <- typeKind typeBindCtx typeCtx to

          Right Kind

  --
  --
  SumT types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx typeCtx) types
          Right Kind

  --
  --
  ProductT types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx typeCtx) types
          Right Kind

  --
  --
  UnionT types
    -> do -- every type must kind-check
          mapM_ (typeKind typeBindCtx typeCtx) (Set.toList types)
          Right Kind

  --
  --
  BigArrow fromKy toTy
    -> do let newTypeBindCtx = addBinding fromKy typeBindCtx
          -- TODO: should this be KindArrow?? like TypeLam.
          -- Is that what makes them different or should they be the same thing???
          _ <- typeKind newTypeBindCtx typeCtx toTy
          Right Kind
  --
  --
  TypeBinding b
    -> case lookupBindingTy b typeBindCtx of
         Nothing
           -> Left $ EContext (EMsg $ text "Kind-checking type") $ EBindCtxTypeLookupFailure (fromEnum b) typeBindCtx
         Just ky
           -> Right ky

  --
  --
  TypeLam absKy ty
    -> do -- type must kind-check under the introduction of a new abstraction to the typeBindCtx
          let newTypeBindCtx = addBinding absKy typeBindCtx
          tyKy <- typeKind newTypeBindCtx typeCtx ty
          Right $ KindArrow absKy tyKy

  --
  --
  TypeApp fTy xTy
    -> do -- both f and x must kind-check
          fKy <- typeKind typeBindCtx typeCtx fTy
          xKy <- typeKind typeBindCtx typeCtx xTy

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

