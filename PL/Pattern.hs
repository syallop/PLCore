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
Module      : PL.Pattern
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PL.Pattern
  ( Pattern (..)
  , pattern SumPattern
  , pattern SumPatternExt
  , pattern ProductPattern
  , pattern ProductPatternExt
  , pattern EmptyProductPattern
  , pattern EmptyProductPatternExt
  , pattern UnionPattern
  , pattern UnionPatternExt
  , pattern BindingPattern
  , pattern BindingPatternExt
  , pattern Bind
  , pattern BindExt
  , pattern PatternExtension
  , pattern PatternExtensionExt

  , PatternFor
  , PatternF (..)

  , branchType
  , checkWithPattern
  , checkWithPatterns

  , SumPatternExtension
  , ProductPatternExtension
  , UnionPatternExtension
  , BindingPatternExtension
  , BindExtension
  , PatternExtension
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

type Pattern = PatternFor DefaultPhase

type PatternFor phase = FixPhase phase PatternF

-- | Argument pattern in a case statements pattern.
-- case ... of
--  T {A b (C d E)} -> ...
data PatternF phase pat
  = SumPatternF
      { _patternSumExtension :: SumPatternExtension phase
      , _index               :: Int   -- ^ The index of the type within the sum we wish to pattern
      , _pattern             :: pat -- ^  within the sumPattern
      }

  | ProductPatternF
      { _patternProductExtension :: ProductPatternExtension phase
      , _patterns                :: [pat] -- ^  against each of the products valuesPattern
      }

  | UnionPatternF
      { _patternUnionExtension :: UnionPatternExtension phase
      , _typeIndex             :: TypeFor phase  -- ^ The index of the type within the union we weish to pattern
      , _pattern               :: pat          -- ^  within the unionPattern
      }

  | BindingPatternF
      { _patternBinding :: BindingPatternExtension phase
      , _equalTo        :: BindingFor phase -- ^ The value should pattern the value of the binding
      }

  | BindF
      { _bindExtension :: BindExtension phase
      }
    -- ^  anything and bind itPattern

  | PatternExtensionF
      { _patternExtension :: !(PatternExtension phase)
      }

deriving instance
  (Show (SumPatternExtension phase)
  ,Show (ProductPatternExtension phase)
  ,Show (UnionPatternExtension phase)
  ,Show (BindingPatternExtension phase)
  ,Show (BindExtension phase)
  ,Show (PatternExtension phase)
  ,Show (TypeFor phase)
  ,Show (BindingFor phase)
  ,Show pat
  )
  => Show (PatternF phase pat)

deriving instance
  (Eq (SumPatternExtension phase)
  ,Eq (ProductPatternExtension phase)
  ,Eq (UnionPatternExtension phase)
  ,Eq (BindingPatternExtension phase)
  ,Eq (BindExtension phase)
  ,Eq (PatternExtension phase)
  ,Eq (TypeFor phase)
  ,Eq (BindingFor phase)
  ,Eq pat
  )
  => Eq (PatternF phase pat)

deriving instance
  (Ord (SumPatternExtension phase)
  ,Ord (ProductPatternExtension phase)
  ,Ord (UnionPatternExtension phase)
  ,Ord (BindingPatternExtension phase)
  ,Ord (BindExtension phase)
  ,Ord (PatternExtension phase)
  ,Ord (TypeFor phase)
  ,Ord (BindingFor phase)
  ,Ord pat
  )
  => Ord (PatternF phase pat)

-- The type families below allow adding new parameters to each of the
-- base constructors of an expression which depend upon the phase
type family SumPatternExtension phase
type family ProductPatternExtension phase
type family UnionPatternExtension phase
type family BindingPatternExtension phase
type family BindExtension phase

-- The PatternExtension type family allows adding new constructors to the base
-- Pattern type which depend upon the phase
type family PatternExtension phase

-- SumF for phases where there is no extension to the constructor.Pattern
pattern SumPattern :: SumPatternExtension phase ~ Void => Int -> PatternFor phase -> PatternFor phase
pattern SumPattern ix pat <- FixPhase (SumPatternF _ ix pat)
  where SumPattern ix pat =  FixPhase (SumPatternF void ix pat)

pattern SumPatternExt :: SumPatternExtension phase -> Int -> PatternFor phase -> PatternFor phase
pattern SumPatternExt ext ix pat <- FixPhase (SumPatternF ext ix pat)
  where SumPatternExt ext ix pat =  FixPhase (SumPatternF ext ix pat)

-- ProductF for phases where there is no extension to the constructor.Pattern
pattern ProductPattern :: ProductPatternExtension phase ~ Void => [PatternFor phase] -> PatternFor phase
pattern ProductPattern pates <- FixPhase (ProductPatternF _ pates)
  where ProductPattern pates =  FixPhase (ProductPatternF void pates)

-- ProductF for the empty product in phase with no extension to thePattern
-- constructor.
pattern EmptyProductPattern :: ProductPatternExtension phase ~ Void => PatternFor phase
pattern EmptyProductPattern <- FixPhase (ProductPatternF _ [])
  where EmptyProductPattern =  FixPhase (ProductPatternF void [])

pattern ProductPatternExt :: ProductPatternExtension phase -> [PatternFor phase] -> PatternFor phase
pattern ProductPatternExt ext pates <- FixPhase (ProductPatternF ext pates)
  where ProductPatternExt ext pates =  FixPhase (ProductPatternF ext pates)

pattern EmptyProductPatternExt :: ProductPatternExtension phase -> PatternFor phase
pattern EmptyProductPatternExt ext <- FixPhase (ProductPatternF ext [])
  where EmptyProductPatternExt ext =  FixPhase (ProductPatternF ext [])

-- UnionF for phases where there is no extension to the constructor.Pattern
pattern UnionPattern :: UnionPatternExtension phase ~ Void => TypeFor phase -> PatternFor phase -> PatternFor phase
pattern UnionPattern typeIx pat <- FixPhase (UnionPatternF _ typeIx pat)
  where UnionPattern typeIx pat =  FixPhase (UnionPatternF void typeIx pat)

pattern UnionPatternExt :: UnionPatternExtension phase -> TypeFor phase -> PatternFor phase -> PatternFor phase
pattern UnionPatternExt ext typeIx pat <- FixPhase (UnionPatternF ext typeIx pat)
  where UnionPatternExt ext typeIx pat =  FixPhase (UnionPatternF ext typeIx pat)

-- BindingPatternF for phases where there is no extension to the constructor.Pattern
pattern BindingPattern :: BindingPatternExtension phase ~ Void => BindingFor phase -> PatternFor phase
pattern BindingPattern equalTo <- FixPhase (BindingPatternF _ equalTo)
  where BindingPattern equalTo =  FixPhase (BindingPatternF void equalTo)

pattern BindingPatternExt :: BindingPatternExtension phase -> BindingFor phase -> PatternFor phase
pattern BindingPatternExt ext equalTo <- FixPhase (BindingPatternF ext equalTo)
  where BindingPatternExt ext equalTo =  FixPhase (BindingPatternF ext equalTo)

-- BindF for phases where there is no extension to the constructor.
pattern Bind :: BindExtension phase ~ Void => PatternFor phase
pattern Bind <- FixPhase (BindF _)
  where Bind =  FixPhase (BindF void)

pattern BindExt :: BindExtension phase -> PatternFor phase
pattern BindExt ext <- FixPhase (BindF ext)
  where BindExt ext =  FixPhase (BindF ext)

-- PatternExtensionF for phases where there is no extension to the number of constructors.
pattern PatternExtension :: PatternExtension phase ~ Void => PatternFor phase
pattern PatternExtension <- FixPhase (PatternExtensionF _)
  where PatternExtension =  FixPhase (PatternExtensionF void)

pattern PatternExtensionExt :: PatternExtension phase -> PatternFor phase
pattern PatternExtensionExt ext <- FixPhase (PatternExtensionF ext)
  where PatternExtensionExt ext =  FixPhase (PatternExtensionF ext)

-- The DefaultPhase has no extensions to constructors or the Pattern itself
type instance SumPatternExtension DefaultPhase = Void
type instance ProductPatternExtension DefaultPhase = Void
type instance UnionPatternExtension DefaultPhase = Void
type instance BindingPatternExtension DefaultPhase = Void
type instance BindExtension DefaultPhase = Void

-- TODO: Should _this_ be unit instead of void?
type instance PatternExtension DefaultPhase = Void

-- Type check a case branch, requiring it pattern the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: (BindingFor DefaultPhase ~ Var)
  => CaseBranch expr Pattern
  -> (BindCtx Var Type -> BindCtx TyVar Kind -> Bindings Type -> TypeCtx DefaultPhase -> expr -> Either (Error Type Pattern) Type)
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type Pattern) Type
branchType (CaseBranch lhs rhs) exprType expectedTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  bindings <- checkWithPattern lhs expectedTy exprBindCtx typeBindCtx typeBindings typeCtx
  exprType (addBindings bindings exprBindCtx) typeBindCtx typeBindings typeCtx rhs

-- | Check that a Pattern patternes the expected Type
-- If so, return a list of types of any bound bindings.
checkWithPattern
  :: (BindingFor DefaultPhase ~ Var)
  => Pattern
  -> Type
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type (PatternFor DefaultPhase)) [Type]
checkWithPattern pat expectTy exprBindCtx typeBindCtx typeBindings typeCtx = do
  rExpectTy <- either (\name -> Left $ ETypeNotDefined name "expected type in a pattern.") Right $ _typeInfoType <$> resolveTypeInitialInfo expectTy typeCtx
  case pat of

    -- Bind the value
    Bind
      -> Right [expectTy]

    BindingPattern b
      -> do -- the type of the binding
            bTy <- maybe (Left $ EMsg $ text "pattern pattern on a non-existant binding") Right $ lookupBindingTy b exprBindCtx
            case typeEq typeBindCtx typeBindings typeCtx bTy expectTy of
                Left err         -> Left err
                Right isSameType -> if isSameType then pure [] else Left $ EMsg $ text "pattern pattern on a binding from a different type"

    SumPattern sumIndex nestedPattern
      -> do sumTypes <- case rExpectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left . EPatternMismatch rExpectTy $ pat

            -- index must be within the number of alternative in the sum type
            patternedTy <- if NE.length sumTypes <= sumIndex
                           then Left $ EMsg $ text "patterning on a larger sum index than the sum type contains"
                           else Right (sumTypes NE.!! sumIndex)

            -- must have the expected index type
            checkWithPattern nestedPattern patternedTy exprBindCtx typeBindCtx typeBindings typeCtx

    ProductPattern nestedPatterns
      -> do prodTypes <- case rExpectTy of
                             ProductT prodTypes
                               -> Right prodTypes
                             _ -> Left $ EMsg $ text "Expected product type in pattern"

            checkWithPatterns nestedPatterns prodTypes exprBindCtx typeBindCtx typeBindings typeCtx

    UnionPattern unionIndexTy nestedPattern
      -> do unionTypes <- case rExpectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg $ text "Expected union type in pattern"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg $ text "Matching on a type which isnt a member of the union"

            -- must actually pattern on the expected type
            checkWithPattern nestedPattern unionIndexTy exprBindCtx typeBindCtx typeBindings typeCtx

    _ -> error "Non-exhaustive pattern when checking pattern statement"

checkWithPatterns
  :: (BindingFor DefaultPhase ~ Var)
  => [Pattern]
  -> [Type]
  -> BindCtx Var Type
  -> BindCtx TyVar Kind
  -> Bindings Type
  -> TypeCtx DefaultPhase
  -> Either (Error Type Pattern) [Type]
checkWithPatterns pat types exprBindCtx typeBindCtx typeBindings typeCtx = case (pat,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg $ text "Expected more patterns in pattern"
  (_,[])  -> Left $ EMsg $ text "Too many patterns in pattern"
  (m:ms,t:ts)
    -> checkWithPattern m t exprBindCtx typeBindCtx typeBindings typeCtx >>= \boundTs -> checkWithPatterns ms ts exprBindCtx typeBindCtx typeBindings typeCtx >>= Right . (boundTs ++)

