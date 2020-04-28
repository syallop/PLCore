{-# LANGUAGE
     ConstraintKinds
   , ConstraintKinds
   , DataKinds
   , DeriveAnyClass
   , EmptyCase
   , FlexibleContexts
   , FlexibleInstances
   , GADTs
   , LambdaCase
   , MultiParamTypeClasses
   , OverloadedStrings
   , PatternSynonyms
   , RankNTypes
   , ScopedTypeVariables
   , StandaloneDeriving
   , TypeFamilies
   , UndecidableInstances
   , TypeOperators
   #-}
{-|
Module      : PL.MatchArg
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.MatchArg
  ( MatchArg (..)
  , pattern MatchSum
  , pattern MatchSumExt
  , pattern MatchProduct
  , pattern MatchProductExt
  , pattern MatchEmptyProduct
  , pattern MatchEmptyProductExt
  , pattern MatchUnion
  , pattern MatchUnionExt
  , pattern MatchBinding
  , pattern MatchBindingExt
  , pattern Bind
  , pattern BindExt
  , pattern MatchArgExtension
  , pattern MatchArgExtensionExt

  , MatchArgFor
  , MatchArgF (..)

  , branchType
  , checkMatchWith
  , checkMatchesWith

  , MatchSumExtension
  , MatchProductExtension
  , MatchUnionExtension
  , MatchBindingExtension
  , BindExtension
  , MatchArgExtension
  )
  where

import PL.FixPhase
import PL.Type
import PL.Kind
import PL.Bindings
import PL.TypeCtx
import PL.TyVar
import PL.Var
import PL.Binds
import PL.Type.Eq
import PL.Error
import PL.Case

import PLPrinter
import PLPrinter.Doc

import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import GHC.Types (Constraint)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type MatchArg = MatchArgFor DefaultPhase

type MatchArgFor phase = FixPhase phase MatchArgF

-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArgF phase match
  = MatchSumF
      { _matchSumExtension :: MatchSumExtension phase
      , _index             :: Int   -- ^ The index of the type within the sum we wish to match
      , _match             :: match -- ^ Match within the sum
      }

  | MatchProductF
      { _matchProductExtension :: MatchProductExtension phase
      , _matches               :: [match] -- ^ Match against each of the products values
      }

  | MatchUnionF
      { _matchUnionExtension :: MatchUnionExtension phase
      , _typeIndex           :: TypeFor phase  -- ^ The index of the type within the union we weish to match
      , _match               :: match          -- ^ Match within the union
      }

  | MatchBindingF
      { _matchBinding :: MatchBindingExtension phase
      , _equalTo      :: BindingFor phase -- ^ The value should match the value of the binding
      }

  | BindF
      { _bindExtension :: BindExtension phase
      }
    -- ^ Match anything and bind it

  | MatchArgExtensionF
      { _matchArgExtension :: !(MatchArgExtension phase)
      }

deriving instance
  (Show (MatchSumExtension phase)
  ,Show (MatchProductExtension phase)
  ,Show (MatchUnionExtension phase)
  ,Show (MatchBindingExtension phase)
  ,Show (BindExtension phase)
  ,Show (MatchArgExtension phase)
  ,Show (TypeFor phase)
  ,Show (BindingFor phase)
  ,Show match
  )
  => Show (MatchArgF phase match)

deriving instance
  (Eq (MatchSumExtension phase)
  ,Eq (MatchProductExtension phase)
  ,Eq (MatchUnionExtension phase)
  ,Eq (MatchBindingExtension phase)
  ,Eq (BindExtension phase)
  ,Eq (MatchArgExtension phase)
  ,Eq (TypeFor phase)
  ,Eq (BindingFor phase)
  ,Eq match
  )
  => Eq (MatchArgF phase match)

deriving instance
  (Ord (MatchSumExtension phase)
  ,Ord (MatchProductExtension phase)
  ,Ord (MatchUnionExtension phase)
  ,Ord (MatchBindingExtension phase)
  ,Ord (BindExtension phase)
  ,Ord (MatchArgExtension phase)
  ,Ord (TypeFor phase)
  ,Ord (BindingFor phase)
  ,Ord match
  )
  => Ord (MatchArgF phase match)

-- The type families below allow adding new parameters to each of the
-- base constructors of an expression which depend upon the phase
type family MatchSumExtension phase
type family MatchProductExtension phase
type family MatchUnionExtension phase
type family MatchBindingExtension phase
type family BindExtension phase

-- The MatchArgExtension type family allows adding new constructors to the base
-- MatchArg type which depend upon the phase
type family MatchArgExtension phase

-- MatchSumF for phases where there is no extension to the constructor.
pattern MatchSum :: MatchSumExtension phase ~ Void => Int -> MatchArgFor phase -> MatchArgFor phase
pattern MatchSum ix match <- FixPhase (MatchSumF _ ix match)
  where MatchSum ix match =  FixPhase (MatchSumF void ix match)

pattern MatchSumExt :: MatchSumExtension phase -> Int -> MatchArgFor phase -> MatchArgFor phase
pattern MatchSumExt ext ix match <- FixPhase (MatchSumF ext ix match)
  where MatchSumExt ext ix match =  FixPhase (MatchSumF ext ix match)

-- MatchProductF for phases where there is no extension to the constructor.
pattern MatchProduct :: MatchProductExtension phase ~ Void => [MatchArgFor phase] -> MatchArgFor phase
pattern MatchProduct matches <- FixPhase (MatchProductF _ matches)
  where MatchProduct matches =  FixPhase (MatchProductF void matches)

-- MatchProductF for the empty product in phase with no extension to the
-- constructor.
pattern MatchEmptyProduct :: MatchProductExtension phase ~ Void => MatchArgFor phase
pattern MatchEmptyProduct <- FixPhase (MatchProductF _ [])
  where MatchEmptyProduct =  FixPhase (MatchProductF void [])

pattern MatchProductExt :: MatchProductExtension phase -> [MatchArgFor phase] -> MatchArgFor phase
pattern MatchProductExt ext matches <- FixPhase (MatchProductF ext matches)
  where MatchProductExt ext matches =  FixPhase (MatchProductF ext matches)

pattern MatchEmptyProductExt :: MatchProductExtension phase -> MatchArgFor phase
pattern MatchEmptyProductExt ext <- FixPhase (MatchProductF ext [])
  where MatchEmptyProductExt ext =  FixPhase (MatchProductF ext [])

-- MatchUnionF for phases where there is no extension to the constructor.
pattern MatchUnion :: MatchUnionExtension phase ~ Void => TypeFor phase -> MatchArgFor phase -> MatchArgFor phase
pattern MatchUnion typeIx match <- FixPhase (MatchUnionF _ typeIx match)
  where MatchUnion typeIx match =  FixPhase (MatchUnionF void typeIx match)

pattern MatchUnionExt :: MatchUnionExtension phase -> TypeFor phase -> MatchArgFor phase -> MatchArgFor phase
pattern MatchUnionExt ext typeIx match <- FixPhase (MatchUnionF ext typeIx match)
  where MatchUnionExt ext typeIx match =  FixPhase (MatchUnionF ext typeIx match)

-- MatchBindingF for phases where there is no extension to the constructor.
pattern MatchBinding :: MatchBindingExtension phase ~ Void => BindingFor phase -> MatchArgFor phase
pattern MatchBinding equalTo <- FixPhase (MatchBindingF _ equalTo)
  where MatchBinding equalTo =  FixPhase (MatchBindingF void equalTo)

pattern MatchBindingExt :: MatchBindingExtension phase -> BindingFor phase -> MatchArgFor phase
pattern MatchBindingExt ext equalTo <- FixPhase (MatchBindingF ext equalTo)
  where MatchBindingExt ext equalTo =  FixPhase (MatchBindingF ext equalTo)

-- BindF for phases where there is no extension to the constructor.
pattern Bind :: BindExtension phase ~ Void => MatchArgFor phase
pattern Bind <- FixPhase (BindF _)
  where Bind =  FixPhase (BindF void)

pattern BindExt :: BindExtension phase -> MatchArgFor phase
pattern BindExt ext <- FixPhase (BindF ext)
  where BindExt ext =  FixPhase (BindF ext)

-- MatchArgExtensionF for phases where there is no extension to the number of constructors.
pattern MatchArgExtension :: MatchArgExtension phase ~ Void => MatchArgFor phase
pattern MatchArgExtension <- FixPhase (MatchArgExtensionF _)
  where MatchArgExtension =  FixPhase (MatchArgExtensionF void)

pattern MatchArgExtensionExt :: MatchArgExtension phase -> MatchArgFor phase
pattern MatchArgExtensionExt ext <- FixPhase (MatchArgExtensionF ext)
  where MatchArgExtensionExt ext =  FixPhase (MatchArgExtensionF ext)

-- The DefaultPhase has no extensions to constructors or the MatchArg itself
type instance MatchSumExtension DefaultPhase = Void
type instance MatchProductExtension DefaultPhase = Void
type instance MatchUnionExtension DefaultPhase = Void
type instance MatchBindingExtension DefaultPhase = Void
type instance BindExtension DefaultPhase = Void

-- TODO: Should _this_ be unit instead of void?
type instance MatchArgExtension DefaultPhase = Void

-- Type check a case branch, requiring it match the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: (BindingFor DefaultPhase ~ Var)
  => CaseBranch expr MatchArg
  -> (BindCtx Var Type -> BindCtx TyVar Kind -> Bindings Type -> TypeCtx DefaultPhase -> expr -> Either (Error Type MatchArg) Type)
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type MatchArg) Type
branchType (CaseBranch lhs rhs) exprType expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkMatchWith lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs

-- | Check that a MatchArg matches the expected Type
-- If so, return a list of types of any bound bindings.
checkMatchWith
  :: (BindingFor DefaultPhase ~ Var)
  => MatchArg
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type (MatchArgFor DefaultPhase)) [Type]
checkMatchWith match expectTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  rExpectTy <- either (\name -> Left $ ETypeNotDefined name "expected type in a pattern.") Right $ _typeInfoType <$> resolveTypeInitialInfo expectTy typeCtx
  case match of

    -- Bind the value
    Bind
      -> Right [expectTy]

    MatchBinding b
      -> do -- the type of the binding
            bTy <- maybe (Left $ EMsg $ text "pattern match on a non-existant binding") Right $ lookupBindingTy b exprBindCtx
            case typeEq typeBindCtx typeBindings typeCtx bTy expectTy of
                Left err         -> Left err
                Right isSameType -> if isSameType then pure [] else Left $ EMsg $ text "pattern match on a binding from a different type"

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left . EPatternMismatch rExpectTy $ match

            -- index must be within the number of alternative in the sum type
            matchedTy <- if NE.length sumTypes <= sumIndex
                           then Left $ EMsg $ text "Matching on a larger sum index than the sum type contains"
                           else Right (sumTypes NE.!! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy exprBindCtx typeBindCtx typeBindings typeCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case rExpectTy of
                             ProductT prodTypes
                               -> Right prodTypes
                             _ -> Left $ EMsg $ text "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes exprBindCtx typeBindCtx typeBindings typeCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg $ text "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg $ text "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy exprBindCtx typeBindCtx typeBindings typeCtx

    _ -> error "Non-exhaustive pattern match when checking match statement"

checkMatchesWith
  :: (BindingFor DefaultPhase ~ Var)
  => [MatchArg]
  -> [Type]
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type MatchArg) [Type]
checkMatchesWith matches types exprBindCtx typeBindCtx typeBindings typeCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg $ text "Expected more patterns in match"
  (_,[])  -> Left $ EMsg $ text "Too many patterns in match"
  (m:ms,t:ts)
    -> checkMatchWith m t exprBindCtx typeBindCtx typeBindings typeCtx >>= \boundTs -> checkMatchesWith ms ts exprBindCtx typeBindCtx typeBindings typeCtx >>= Right . (boundTs ++)

