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
  , matchableType

  , SumPatternExtension
  , ProductPatternExtension
  , UnionPatternExtension
  , BindingPatternExtension
  , BindExtension
  , PatternExtension
  )
  where

-- PL
import PL.Bindings
import PL.Binds
import PL.Binds.Ix
import PL.Case
import PL.Error
import PL.FixPhase
import PL.Kind
import PL.ReduceType
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCheck
import PL.TypeCtx
import PL.Var

-- External PL
import PLHash
import PLPrinter
import PLPrinter.Doc

-- Other
import Control.Applicative
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding (Sum,Product)
import Data.Proxy
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

instance
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
  => Show (PatternF phase pat) where
  show p = mconcat $ case p of
    SumPatternF ext ix p
      -> ["{Sum ", show ext, " ", show ix ," ", show p, "}"]
    ProductPatternF ext ps
      -> ["{Product ", show ext, " ", show ps, "}"]
    UnionPatternF ext tyIx p
      -> ["{Union ", show ext, " ", show tyIx, " ", show p, "}"]
    BindingPatternF ext b
      -> ["{Binding ", show ext, " ", show b, "}"]
    BindF ext
      -> ["{Bind ", show ext, "}"]
    PatternExtensionF ext
      -> ["{Extension ", show ext, "}"]

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

instance
  (Hashable (SumPatternExtension phase)
  ,Hashable (ProductPatternExtension phase)
  ,Hashable (UnionPatternExtension phase)
  ,Hashable (BindingPatternExtension phase)
  ,Hashable (BindExtension phase)
  ,Hashable (PatternExtension phase)
  ,Hashable (TypeFor phase)
  ,Hashable (BindingFor phase)
  ,Hashable pat
  )
  => Hashable (PatternF phase pat) where
  toHashToken p = case p of
    SumPatternF ext ix p
      -> HashTag "Pattern.Sum" [toHashToken ext, HashInt ix, toHashToken p]

    ProductPatternF ext ps
      -> HashTag "Pattern.Product" [toHashToken ext, toHashToken ps]

    UnionPatternF ext tyIx p
      -> HashTag "Pattern.Union" [toHashToken ext, toHashToken tyIx, toHashToken p]

    BindingPatternF ext b
      -> HashTag "Pattern.Binding" [toHashToken ext, toHashToken b]

    BindF ext
      -> HashTag "Pattern.Bind" [toHashToken ext]

    PatternExtensionF ext
      -> HashTag "Pattern.Extension" [toHashToken ext]

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
pattern SumPattern :: SumPatternExtension phase ~ NoExt => Int -> PatternFor phase -> PatternFor phase
pattern SumPattern ix pat <- FixPhase (SumPatternF _ ix pat)
  where SumPattern ix pat =  FixPhase (SumPatternF noExt ix pat)

pattern SumPatternExt :: SumPatternExtension phase -> Int -> PatternFor phase -> PatternFor phase
pattern SumPatternExt ext ix pat <- FixPhase (SumPatternF ext ix pat)
  where SumPatternExt ext ix pat =  FixPhase (SumPatternF ext ix pat)

-- ProductF for phases where there is no extension to the constructor.Pattern
pattern ProductPattern :: ProductPatternExtension phase ~ NoExt => [PatternFor phase] -> PatternFor phase
pattern ProductPattern pates <- FixPhase (ProductPatternF _ pates)
  where ProductPattern pates =  FixPhase (ProductPatternF noExt pates)

-- ProductF for the empty product in phase with no extension to thePattern
-- constructor.
pattern EmptyProductPattern :: ProductPatternExtension phase ~ NoExt => PatternFor phase
pattern EmptyProductPattern <- FixPhase (ProductPatternF _ [])
  where EmptyProductPattern =  FixPhase (ProductPatternF noExt [])

pattern ProductPatternExt :: ProductPatternExtension phase -> [PatternFor phase] -> PatternFor phase
pattern ProductPatternExt ext pates <- FixPhase (ProductPatternF ext pates)
  where ProductPatternExt ext pates =  FixPhase (ProductPatternF ext pates)

pattern EmptyProductPatternExt :: ProductPatternExtension phase -> PatternFor phase
pattern EmptyProductPatternExt ext <- FixPhase (ProductPatternF ext [])
  where EmptyProductPatternExt ext =  FixPhase (ProductPatternF ext [])

-- UnionF for phases where there is no extension to the constructor.Pattern
pattern UnionPattern :: UnionPatternExtension phase ~ NoExt => TypeFor phase -> PatternFor phase -> PatternFor phase
pattern UnionPattern typeIx pat <- FixPhase (UnionPatternF _ typeIx pat)
  where UnionPattern typeIx pat =  FixPhase (UnionPatternF noExt typeIx pat)

pattern UnionPatternExt :: UnionPatternExtension phase -> TypeFor phase -> PatternFor phase -> PatternFor phase
pattern UnionPatternExt ext typeIx pat <- FixPhase (UnionPatternF ext typeIx pat)
  where UnionPatternExt ext typeIx pat =  FixPhase (UnionPatternF ext typeIx pat)

-- BindingPatternF for phases where there is no extension to the constructor.Pattern
pattern BindingPattern :: BindingPatternExtension phase ~ NoExt => BindingFor phase -> PatternFor phase
pattern BindingPattern equalTo <- FixPhase (BindingPatternF _ equalTo)
  where BindingPattern equalTo =  FixPhase (BindingPatternF noExt equalTo)

pattern BindingPatternExt :: BindingPatternExtension phase -> BindingFor phase -> PatternFor phase
pattern BindingPatternExt ext equalTo <- FixPhase (BindingPatternF ext equalTo)
  where BindingPatternExt ext equalTo =  FixPhase (BindingPatternF ext equalTo)

-- BindF for phases where there is no extension to the constructor.
pattern Bind :: BindExtension phase ~ NoExt => PatternFor phase
pattern Bind <- FixPhase (BindF _)
  where Bind =  FixPhase (BindF noExt)

pattern BindExt :: BindExtension phase -> PatternFor phase
pattern BindExt ext <- FixPhase (BindF ext)
  where BindExt ext =  FixPhase (BindF ext)

-- PatternExtensionF for phases where there is no extension to the number of constructors.
pattern PatternExtension :: PatternExtension phase ~ NoExt => PatternFor phase
pattern PatternExtension <- FixPhase (PatternExtensionF _)
  where PatternExtension =  FixPhase (PatternExtensionF noExt)

pattern PatternExtensionExt :: PatternExtension phase -> PatternFor phase
pattern PatternExtensionExt ext <- FixPhase (PatternExtensionF ext)
  where PatternExtensionExt ext =  FixPhase (PatternExtensionF ext)

-- The DefaultPhase has no extensions to constructors or the Pattern itself
type instance SumPatternExtension DefaultPhase = NoExt
type instance ProductPatternExtension DefaultPhase = NoExt
type instance UnionPatternExtension DefaultPhase = NoExt
type instance BindingPatternExtension DefaultPhase = NoExt
type instance BindExtension DefaultPhase = NoExt

-- TODO: Should _this_ be unit instead of noExt?
type instance PatternExtension DefaultPhase = NoExt

-- Type check a case branch, requiring it pattern the expected type
-- , if so, type checking the result expression which is returned.
branchType
  :: CaseBranch expr Pattern
  -> (TypeCheckCtx -> expr -> Either Error Type)
  -> Type
  -> TypeCheckCtx
  -> Either Error Type
branchType (CaseBranch lhs rhs) exprType expectedTy ctx = do
  bindings <- checkWithPattern lhs expectedTy ctx
  exprType (underExpressionAbstractions bindings ctx) rhs

-- | Check that a Pattern patternes the expected Type
-- If so, return a list of types of any bound bindings.
checkWithPattern
  :: Pattern
  -> Type
  -> TypeCheckCtx
  -> Either Error [Type]
checkWithPattern pat expectTy ctx = either (Left . EContext (EPatternMismatch expectTy pat)) Right $ do
  (ctx, matchTy) <- matchableType expectTy ctx
  case (pat, matchTy) of

    -- Bindings always match and bind the value.
    (Bind
     ,matchTy)
      -> Right [matchTy]

    -- BindingPatterns match when the binding is bound and has the same type.
    (BindingPattern b
     , matchTy)
      -> do -- the type of the binding
            bTy <- lookupVarType b ctx
            case checkEqual bTy matchTy ctx of
                Left err
                  -> Left err

                Right isSameType
                  -> if isSameType
                       then pure []
                       else Left $ EMsg $ text "pattern pattern on a binding from a different type"

    (SumPattern sumIndex nestedPattern
     , SumT sumTypes)
      -> do -- index must be within the number of alternative in the sum type
            patternedTy <- if NE.length sumTypes <= sumIndex
                           then Left $ EMsg $ text "patterning on a larger sum index than the sum type contains"
                           else Right (sumTypes NE.!! sumIndex)

            -- must have the expected index type
            checkWithPattern nestedPattern patternedTy ctx

    (ProductPattern nestedPatterns
     , ProductT prodTypes)
      -> checkWithPatterns nestedPatterns prodTypes ctx

    -- Type index must be equal to exactly one of the alternatives.
    (UnionPattern unionIndexTy nestedPattern
     , UnionT unionTypes)
      -> do -- Search through the set of possible types collecting:
            -- - Matching types
            -- - Or any errors when checking type equality
            let searchResult = Set.foldr (\unionTy accResult -> case accResult of
                                            -- Errors short-circuit the search.
                                            Left err
                                              -> Left err
                                            Right s
                                              -> case checkEqual unionIndexTy unionTy ctx of
                                                   Left err
                                                     -> (Left err)

                                                   Right False
                                                     -> Right s

                                                   Right True
                                                     -> Right (Set.insert unionTy s)
                                          )
                                          (Right Set.empty)
                                          unionTypes
            -- If we encountered an error checking any two types, report it,
            -- otherwise enforce that exactly one match was made.
            () <- case searchResult of
              Left err
                -> Left err

              Right matches
                -> case Set.toList matches of
                     []
                       -> Left  . EMsg . text $ "Zero types matched"

                     [_]
                       -> Right ()

                     matches
                       -> Left $ EMultipleMatchesInUnion matches

            -- must actually pattern on the expected type
            checkWithPattern nestedPattern unionIndexTy ctx

    (pat
     , matchTy)
      -> Left . EMsg . text $ "Pattern matches a different type"

checkWithPatterns
  :: [Pattern]
  -> [Type]
  -> TypeCheckCtx
  -> Either Error [Type]
checkWithPatterns pat types ctx = case (pat,types) of
  ([]
   ,[])
    -> Right []

  ([]
   ,_)
    -> Left $ EMsg $ text "Expected more patterns in pattern"

  (_
   ,[])
    -> Left $ EMsg $ text "Too many patterns in pattern"

  (p:ps
   , t:ts)
    -> do boundTs  <- checkWithPattern p t ctx
          boundTs' <- checkWithPatterns ps ts ctx
          Right (boundTs ++ boundTs')

-- | Normalise a type to a form where a single layer of pattern matching can be
-- performed upon it.
--
-- Mu types and Named types have no direct matching construct.
--
-- Instead we expand them so they can be matched by their definitions.
-- I.E.:
-- - Names are replaced with their initial definition
-- - Mu types are deconstructed to their itself-type
--
-- Malformed types will diverge, E.G.
-- - Mu types which refer to themselves without an additional constructor
-- - Named types which refer to themselves
--
-- The types supplied to this function must be well-formed.
matchableType
  :: Type
  -> TypeCheckCtx
  -> Either Error (TypeCheckCtx, Type)
matchableType expectTy ctx
  | isMatchable expectTy = Right (ctx, expectTy)
  | otherwise            = do (newSelf, reducedTy) <- do ty0 <- unBinding expectTy ctx
                                                         ty1 <- unName ty0 ctx

                                                         ty2 <- unSelf ty1 ctx
                                                         unMu ty2 ctx
                              matchableType reducedTy (ctx{_selfType = newSelf})

  where
    -- Is a type matchable, or does it need simplifying first?
    isMatchable :: Type -> Bool
    isMatchable ty = case ty of
      NamedExt _ _
        -> False
      TypeSelfBindingExt _
        -> False
      TypeBindingExt _ _
        -> False
      TypeMuExt _ _ _
        -> False

      _ -> True

    -- Resolve a name to it's initial definition
    unName :: Type -> TypeCheckCtx -> Either Error Type
    unName ty ctx = case ty of
      NamedExt _ n
        -> case lookupTypeNameInitialType n (_typeCtx ctx) of
             Nothing
               -> Left . EContext (EMsg $ text "Checking pattern") . ETypeNotDefined n . _typeCtx $ ctx

             Just ty
               -> case ty of
                    TypeSelfBinding
                      -> Left . EMsg . mconcat $
                           [ text "Name resolved to a plain self type which shouldnt happen."
                           , lineBreak
                           , indent1 . string . show $ n
                           , lineBreak
                           , indent1 . string . show . _typeCtx $ ctx
                           ]
                    _ -> Right ty

      ty
        -> Right ty

    -- Destruct a Mu type into its definition with a single layer of
    -- self-references substituted.
    unMu :: Type -> TypeCheckCtx -> Either Error (Maybe Type, Type)
    unMu ty ctx = case ty of
      TypeMuExt ext kind itselfTy
        -> do destructedTy <- destruct (ext, kind, itselfTy)
              pure $ (Just ty, destructedTy)
      ty
        -> Right (_selfType ctx, ty)

    -- Replace a self-type with a value bound by a Mu and smuggled through the
    -- context.
    unSelf :: Type -> TypeCheckCtx -> Either Error Type
    unSelf ty ctx = case ty of
      TypeSelfBindingExt _
        -> case _selfType ctx of
             Nothing
               -> Left . EMsg . text $ "No self-type in context"

             Just selfTy
               -> Right selfTy

      ty
        -> Right ty

    -- Type bindings _should_ have been resolved by this point.
    --
    -- If not but we have a binding, proceed. Otherwise we cannot perform a
    -- match.
    unBinding :: Type -> TypeCheckCtx -> Either Error Type
    unBinding ty ctx = case ty of
      TypeBindingExt _ tyVar
        -> case safeIndex (Proxy :: Proxy TyVar) (_typeBindings ctx) (bindDepth tyVar) of
             Nothing
               -> Left . EMsg . text $ "Cannot pattern match on an expression whose type is a type-binding because the bindings index does not exist"

             Just Unbound
               -> Left . EMsg . text $ "Cannot pattern match on an expression whose type is a type-binding because the binding is unbound"

             Just (Bound ty)
               -> Right ty

      ty -> Right ty

type instance ErrorPattern DefaultPhase = Pattern

