{-# LANGUAGE
    ConstraintKinds
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
  , TypeOperators
  , UndecidableInstances
  #-}
{-|
Module      : PL.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Types inhabited by Expressions. Types are structural until explicitly Named
and can abstract and be applied much like Expressions.
-}
module PL.Type
  ( Type
  , pattern Named
  , pattern NamedExt
  , pattern Arrow
  , pattern ArrowExt
  , pattern SumT
  , pattern SumTExt
  , pattern ProductT
  , pattern ProductTExt
  , pattern EmptyProductT
  , pattern EmptyProductTExt
  , pattern UnionT
  , pattern UnionTExt
  , pattern BigArrow
  , pattern BigArrowExt
  , pattern TypeLam
  , pattern TypeLamExt
  , pattern TypeApp
  , pattern TypeAppExt
  , pattern TypeBinding
  , pattern TypeBindingExt
  , pattern TypeExtension
  , pattern TypeExtensionExt

  , TypeFor
  , TypeF (..)

  , NamedExtension
  , ArrowExtension
  , SumTExtension
  , ProductTExtension
  , UnionTExtension
  , BigArrowExtension
  , TypeLamExtension
  , TypeAppExtension
  , TypeBindingExtension

  , TypeExtension

  , TypeBindingFor

  , isType
  , (-->)
  , ty
  , arrowise
  , unarrowise
  , instantiate

  -- TODO: Relocate
  , DefaultPhase
  , Void
  , void

  , forgetTypeExtensions
  )
  where

import PL.Binds.Ix
import PL.Name
import PL.Kind
import PL.ExprLike
import PL.FixPhase
import PL.TyVar

import PLGrammar

import PLPrinter hiding (parens,between)
import PLPrinter.Doc

import Data.List
import qualified Data.Set as Set
import Data.Proxy
import Data.Monoid
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as Text

type Type = TypeFor DefaultPhase

-- | Types classify 'Expr'essions.
--
-- - 'tb' is the type of bindings which refer to some abstraction.
--   This could be a variable name "sometype".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to a type abstraction "0","1", etc.
type TypeFor phase = FixPhase phase TypeF

-- | Types classify 'Expr'essions.
--
-- - 'tb' is the type of bindings which refer to some abstraction.
--   This could be a variable name "sometype".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to a type abstraction "0","1", etc.
--
-- - 'typ' is the recursive type of subexpressions. This is usually instantiated
--   using FixPhase such that it refers to itself.
--   This allows us to use recursion schemes.
--
--   Any examples used in constructor comments use completly arbitrary syntax.
data TypeF phase typ

  -- | Named type.
  --
  -- A name which refers to a type definition tracked in some external type context.
  = NamedF
    { _namedExtension :: NamedExtension phase
    , _hasType        :: TypeName
    }

  -- | Arrow is the type of expression functions.
  --
  -- E.G. 'Expr's 'Lam' constructor representing:
  --
  -- \x::Bool x
  --
  -- Might have type:
  --
  -- `Arrow Bool Bool`
  | ArrowF
    { _arrowExtension :: ArrowExtension phase
    , _from           :: typ
    , _to             :: typ
    }

  -- | SumT is the type of expressions which are an ordered alternative of
  -- types.
  --
  -- E.G. 'Expr's 'Sum' constructor representing:
  --
  -- true :: 1 :: Bool|Int|Char
  --
  -- Might have type:
  --
  -- `SumT [Bool,Int,Char]`
  | SumTF
    { _sumExtension :: SumTExtension phase
    , _sumTypes     :: NonEmpty typ
    }

  -- | ProductT is the type of expressions which are an ordered product of each
  -- of the given types.
  --
  -- E.G. 'Expr's 'Product' constructor representing:
  --
  -- (true, 1, 'a')
  --
  -- Might have type:
  --
  -- `ProductT [Bool,Int,Char]`
  | ProductTF
    { _productExtension :: ProductTExtension phase
    , _productTypes     :: [typ]
    }

  -- | UnionT is the type of expressions which are part of a set types.
  --
  -- E.G. 'Expr's 'Union' constructor representing:
  --
  -- x :: Bool :: {Char, Bool, Int}
  --
  -- Might have type:
  --
  -- `UnionT $ set.FromList [Int,Char,Bool]`
  | UnionTF
    { _unionExtension :: UnionTExtension phase
    , _unionTypes     :: Set.Set typ
    }

  -- | BigArrow is the type of expressions which abstract a _Type_ into an
  -- expression.
  --
  -- E.G. 'Expr's 'BigLam' constructor representing:
  --
  -- \(t :: Kind). 1
  --
  -- Might have type:
  --
  -- `BigArrow KIND Int`
  --
  -- TODO: How is this distinct from TypeLam??
  | BigArrowF
    { _bigArrowExtension :: BigArrowExtension phase
    , _takeType          :: Kind
    , _type              :: typ
    }

  -- | TypeLam is a lambda abstraction performed at the type level.
  --
  -- E.G.
  --
  -- \Kind. 0
  --
  -- TODO: Is being able to bind types at the expression and type level
  -- problematic?
  | TypeLamF
    { _typeLamExtension :: TypeLamExtension phase
    , _takeType         :: Kind
    , _type             :: typ
    }

  -- | Type application.
  --
  -- Apply a type to another.
  --
  -- E.G.
  -- (\t::Kind -> t) Bool
  | TypeAppF
    { _typeAppExtension :: TypeAppExtension phase
    , _f                :: typ
    , _x                :: typ
    }

  -- | Type bindings.
  --
  -- Bind the type refered to by 'tb'.
  --
  -- E.G.
  --
  -- 0
  --
  -- where 0 counts the number of `TypeLam`s away a type was abstracted.
  | TypeBindingF
    { _bindingExtension :: TypeBindingExtension phase
    , _binding          :: TypeBindingFor phase
    }

  | TypeExtensionF
    { _typeExtension :: !(TypeExtension phase)
    }

deriving instance
  (Eq (NamedExtension phase)
  ,Eq (ArrowExtension phase)
  ,Eq (SumTExtension phase)
  ,Eq (ProductTExtension phase)
  ,Eq (UnionTExtension phase)
  ,Eq (BigArrowExtension phase)
  ,Eq (TypeLamExtension phase)
  ,Eq (TypeAppExtension phase)
  ,Eq (TypeBindingExtension phase)
  ,Eq (TypeExtension phase)
  ,Eq (TypeBindingFor phase)
  ,Eq typ
  )
  => Eq (TypeF phase typ)

deriving instance
  (Ord (NamedExtension phase)
  ,Ord (ArrowExtension phase)
  ,Ord (SumTExtension phase)
  ,Ord (ProductTExtension phase)
  ,Ord (UnionTExtension phase)
  ,Ord (BigArrowExtension phase)
  ,Ord (TypeLamExtension phase)
  ,Ord (TypeAppExtension phase)
  ,Ord (TypeBindingExtension phase)
  ,Ord (TypeExtension phase)
  ,Ord (TypeBindingFor phase)
  ,Ord typ
  )
  => Ord (TypeF phase typ)

instance
  (Show (NamedExtension phase)
  ,Show (ArrowExtension phase)
  ,Show (SumTExtension phase)
  ,Show (ProductTExtension phase)
  ,Show (UnionTExtension phase)
  ,Show (BigArrowExtension phase)
  ,Show (TypeLamExtension phase)
  ,Show (TypeAppExtension phase)
  ,Show (TypeBindingExtension phase)
  ,Show (TypeExtension phase)
  ,Show (TypeBindingFor phase)
  ,Show typ
  )
  => Show (TypeF phase typ) where
    show t = mconcat $ case t of
      NamedF ext t
        -> ["{Named ", show ext, " ", show t, "}"]

      ArrowF ext from to
        -> ["{Arrow ", show ext, " ", show from, " ", show to, "}"]

      SumTF ext tys
        -> ["{SumT ", show ext, " ", show tys, "}"]

      ProductTF ext tys
        -> ["{ProductT ", show ext, " ", show tys, "}"]

      UnionTF ext tys
        -> ["{UnionT ", show ext, " ", show tys, "}"]

      BigArrowF ext absKind ty
        -> ["{ArrowT ", show ext, " ", show absKind, " ", show ty, "}"]

      TypeLamF ext absKind ty
        -> ["{TypeLam ", show ext, " ", show absKind, " ", show ty, "}"]

      TypeAppF ext fTy xTy
        -> ["{TypeApp ", show ext, " ", show fTy, " ", show xTy, "}"]

      TypeBindingF ext b
        -> ["{TypeBinding ", show ext, " ", show b, "}"]

      TypeExtensionF ext
        -> ["{TypeExtension ", show ext, "}"]


-- The type families below allow adding new parameters to each of the base
-- constructors of a type which depend upon the phase
type family NamedExtension phase
type family ArrowExtension phase
type family SumTExtension phase
type family ProductTExtension phase
type family UnionTExtension phase
type family BigArrowExtension phase
type family TypeLamExtension phase
type family TypeAppExtension phase
type family TypeBindingExtension phase

-- The TypeExtension type family allows adding new constructors to the base Type
-- which depend upon the phase
type family TypeExtension phase

type family TypeBindingFor phase

data Void

instance Show Void where
  show _ = ""

instance Eq Void where
  _ == _ = True

instance Ord Void where
  compare _ _ = EQ

-- Some patterns to make working with ExprF nicer
void :: Void
void = error "Cannot evaluate Void"

-- NamedF for phases where there is no extension to the constructor.
pattern Named :: NamedExtension phase ~ Void => TypeName -> TypeFor phase
pattern Named name <- FixPhase (NamedF _ name)
  where Named name =  FixPhase (NamedF void name)

pattern NamedExt :: NamedExtension phase -> TypeName -> TypeFor phase
pattern NamedExt ext name <- FixPhase (NamedF ext name)
  where NamedExt ext name =  FixPhase (NamedF ext name)

-- ArrowF for phases where there is no extension to the constructor.
pattern Arrow :: ArrowExtension phase ~ Void => TypeFor phase -> TypeFor phase -> TypeFor phase
pattern Arrow fromTy toTy <- FixPhase (ArrowF _ fromTy toTy)
  where Arrow fromTy toTy =  FixPhase (ArrowF void fromTy toTy)

pattern ArrowExt :: ArrowExtension phase -> TypeFor phase -> TypeFor phase -> TypeFor phase
pattern ArrowExt ext fromTy toTy <- FixPhase (ArrowF ext fromTy toTy)
  where ArrowExt ext fromTy toTy =  FixPhase (ArrowF ext fromTy toTy)

-- SumT for phases where there is no extension to the constructor.
pattern SumT :: SumTExtension phase ~ Void => NonEmpty (TypeFor phase) -> TypeFor phase
pattern SumT types <- FixPhase (SumTF _ types)
  where SumT types =  FixPhase (SumTF void types)

pattern SumTExt :: SumTExtension phase -> NonEmpty (TypeFor phase) -> TypeFor phase
pattern SumTExt ext types <- FixPhase (SumTF ext types)
  where SumTExt ext types =  FixPhase (SumTF ext types)

-- ProductT for phases where there is no extension to the constructor.
pattern ProductT :: ProductTExtension phase ~ Void => [TypeFor phase] -> TypeFor phase
pattern ProductT types <- FixPhase (ProductTF _ types)
  where ProductT types =  FixPhase (ProductTF void types)

-- ProductT for the empty product in phases where there is no extension to the constructor.
pattern EmptyProductT :: ProductTExtension phase ~ Void => TypeFor phase
pattern EmptyProductT <- FixPhase (ProductTF _ [])
  where EmptyProductT =  FixPhase (ProductTF void [])

pattern ProductTExt :: ProductTExtension phase -> [TypeFor phase] -> TypeFor phase
pattern ProductTExt ext types <- FixPhase (ProductTF ext types)
  where ProductTExt ext types =  FixPhase (ProductTF ext types)

pattern EmptyProductTExt :: ProductTExtension phase -> TypeFor phase
pattern EmptyProductTExt ext <- FixPhase (ProductTF ext [])
  where EmptyProductTExt ext =  FixPhase (ProductTF ext [])

-- UnionT for phases where there is no extension to the constructor.
pattern UnionT :: UnionTExtension phase ~ Void => Set.Set (TypeFor phase) -> TypeFor phase
pattern UnionT types <- FixPhase (UnionTF _ types)
  where UnionT types =  FixPhase (UnionTF void types)

pattern UnionTExt :: UnionTExtension phase -> Set.Set (TypeFor phase) -> TypeFor phase
pattern UnionTExt ext types <- FixPhase (UnionTF ext types)
  where UnionTExt ext types =  FixPhase (UnionTF ext types)

-- BigArrow for phases where there is no extension to the constructor.
pattern BigArrow :: BigArrowExtension phase ~ Void => Kind -> TypeFor phase -> TypeFor phase
pattern BigArrow kind ty <- FixPhase (BigArrowF _ kind ty)
  where BigArrow kind ty =  FixPhase (BigArrowF void kind ty)

pattern BigArrowExt :: BigArrowExtension phase -> Kind -> TypeFor phase -> TypeFor phase
pattern BigArrowExt ext kind ty <- FixPhase (BigArrowF ext kind ty)
  where BigArrowExt ext kind ty =  FixPhase (BigArrowF ext kind ty)

-- TypeLam for phases where there is no extension to the constructor.
pattern TypeLam :: TypeLamExtension phase ~ Void => Kind -> TypeFor phase -> TypeFor phase
pattern TypeLam absTy ty <- FixPhase (TypeLamF _ absTy ty)
  where TypeLam absTy ty =  FixPhase (TypeLamF void absTy ty)

pattern TypeLamExt :: TypeLamExtension phase -> Kind -> TypeFor phase -> TypeFor phase
pattern TypeLamExt ext absTy ty <- FixPhase (TypeLamF ext absTy ty)
  where TypeLamExt ext absTy ty =  FixPhase (TypeLamF ext absTy ty)

-- TypeApp for phases where there is no extension to the constructor.
pattern TypeApp :: TypeAppExtension phase ~ Void => TypeFor phase -> TypeFor phase -> TypeFor phase
pattern TypeApp fTy xTy <- FixPhase (TypeAppF _ fTy xTy)
  where TypeApp fTy xTy =  FixPhase (TypeAppF void fTy xTy)

pattern TypeAppExt :: TypeAppExtension phase -> TypeFor phase -> TypeFor phase -> TypeFor phase
pattern TypeAppExt ext fTy xTy <- FixPhase (TypeAppF ext fTy xTy)
  where TypeAppExt ext fTy xTy =  FixPhase (TypeAppF ext fTy xTy)

-- TypeBinding for phases where there is no extension to the constructor.
pattern TypeBinding :: TypeBindingExtension phase ~ Void => TypeBindingFor phase -> TypeFor phase
pattern TypeBinding tyVar <- FixPhase (TypeBindingF _ tyVar)
  where TypeBinding tyVar =  FixPhase (TypeBindingF void tyVar)

pattern TypeBindingExt :: TypeBindingExtension phase -> TypeBindingFor phase -> TypeFor phase
pattern TypeBindingExt ext tyVar <- FixPhase (TypeBindingF ext tyVar)
  where TypeBindingExt ext tyVar =  FixPhase (TypeBindingF ext tyVar)

-- TypeExtensionF for phases where there is no extension to number of constructors.
pattern TypeExtension :: TypeExtension phase ~ Void => TypeFor phase
pattern TypeExtension <- FixPhase (TypeExtensionF _)
  where TypeExtension = FixPhase (TypeExtensionF void)

pattern TypeExtensionExt :: TypeExtension phase -> TypeFor phase
pattern TypeExtensionExt ext <- FixPhase (TypeExtensionF ext)
  where TypeExtensionExt ext = FixPhase (TypeExtensionF ext)

-- Phases
data DefaultPhase

type instance NamedExtension DefaultPhase = Void
type instance ArrowExtension DefaultPhase = Void
type instance SumTExtension DefaultPhase = Void
type instance ProductTExtension DefaultPhase = Void
type instance UnionTExtension DefaultPhase = Void
type instance BigArrowExtension DefaultPhase = Void
type instance TypeLamExtension DefaultPhase = Void
type instance TypeAppExtension DefaultPhase = Void
type instance TypeBindingExtension DefaultPhase = Void

-- TODO: Should _this_ be unit instead of void?
type instance TypeExtension DefaultPhase = Void

type instance TypeBindingFor DefaultPhase = TyVar

-- | Is a Type a simple named type
isType :: Type -> Bool
isType t = case t of
  Named _ -> True
  _       -> False

-- | Infix Arrow
(-->) :: Type -> Type -> Type
a --> b = Arrow a b

-- | Construct a simple named type
ty :: TypeName -> Type
ty = Named

-- PARTIAL
-- [a]   ~> Type a
-- [a,b,c] ~> Arrow a (Arrow b c)
-- etc
arrowise :: [Type] -> Type
arrowise []        = error "Can't arrowise empty list of Types"
arrowise [t]       = t
arrowise (t:t':ts) = t --> arrowise (t':ts)

-- a           ~> [a]
-- a -> b -> c ~> [a,b,c]
-- etc
unarrowise :: Type -> [Type]
unarrowise t = case t of
  Arrow a b
    -> a : unarrowise b
  t -> [t]

-- TODO: Can likely use recursion schemes.

instance HasAbs Type where
  applyToAbs f ty = case ty of
    TypeLam ky ty -> TypeLam ky (f ty)
--  Nope(?)
--  BigArrow ky ty -> BigArrow ky (f ty)
    ty            -> ty

instance HasBinding Type TyVar where
  applyToBinding f ty = case ty of
    TypeBinding tb -> TypeBinding (f tb)
    ty             -> ty

instance HasNonAbs Type where
  applyToNonAbs f ty = case ty of
    Arrow from to
      -> Arrow (f from) (f to)

    SumT types
      -> SumT (fmap f types)

    ProductT types
      -> ProductT (map f types)

    UnionT types
      -> UnionT (Set.fromList . map f . Set.toList $ types)

    BigArrow from to
      -> BigArrow from (f to)

    TypeApp x y
      -> TypeApp (f x) (f y)

    ty -> ty

-- instantiate a type within some other.
instantiate
  :: Type
  -> Type
  -> Type
instantiate = instantiate' 0
  where
    instantiate' :: Int -> Type -> Type -> Type
    instantiate' i instType inType = case inType of
      Arrow from to
        -> Arrow (instantiate' i instType from) (instantiate' i instType to)

      SumT ts
        -> SumT $ fmap (instantiate' i instType) ts

      ProductT ts
        -> ProductT $ map (instantiate' i instType) ts

      UnionT ts
        -> UnionT $ Set.map (instantiate' i instType) ts

      BigArrow from to
        -> BigArrow from (instantiate' i instType to)

      TypeLam from to
        -> TypeLam from (instantiate' (i+1) instType to)

      TypeApp f x
        -> TypeApp (instantiate' i instType f) (instantiate' i instType x)

      TypeBinding tb
        | bindDepth tb == i -> buryBy (Proxy :: Proxy TyVar) instType (BuryDepth i)
        | otherwise         ->  TypeBinding tb

      Named n
        ->  Named n

      _ -> error "Non-exhaustive pattern in type instantiation"

-- Forget all constructor extensions on a Type that uses TyVars for bindings in
-- order to produce a type in the default phase.
forgetTypeExtensions
  :: (TypeBindingFor phase ~ TyVar)
  => TypeFor phase
  -> TypeFor DefaultPhase
forgetTypeExtensions = \case
  NamedExt _ n
    -> Named n

  ArrowExt _ from to
    -> Arrow (forgetTypeExtensions from) (forgetTypeExtensions to)

  SumTExt _ types
    -> SumT (fmap forgetTypeExtensions types)

  ProductTExt _ types
    -> ProductT (fmap forgetTypeExtensions types)

  UnionTExt _ types
    -> UnionT (Set.map forgetTypeExtensions types)

  BigArrowExt _ abs toType
    -> BigArrow abs (forgetTypeExtensions toType)

  TypeLamExt _ abs toType
    -> TypeLam abs (forgetTypeExtensions toType)

  TypeAppExt _ fType xType
    -> TypeApp (forgetTypeExtensions fType) (forgetTypeExtensions xType)

  TypeBindingExt _ binding
    -> TypeBinding binding

  TypeExtensionExt _
    -> TypeExtension

  _ -> error "Non-exhaustive pattern forgetting type extensions"

