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
  , pattern TypeSelfBinding
  , pattern TypeSelfBindingExt
  , pattern TypeMu
  , pattern TypeMuExt
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
  , pattern TypeContentBinding
  , pattern TypeContentBindingExt
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
  , TypeContentBindingExtension

  , TypeExtension

  , TypeBindingFor
  , TypeContentBindingFor

  , isType
  , (-->)
  , ty
  , arrowise
  , unarrowise
  , instantiate

  , gatherTypeContentNames

  , forgetTypeExtensions
  )
  where

import Prelude hiding (abs)

-- PL
import PL.Binds.Ix
import PL.Error
import PL.ExprLike
import PL.FixPhase
import PL.Kind
import PL.Name
import PL.TyVar

-- External PL
import PLHash

-- Other
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set

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

  -- | Abstract a type over itself
  | TypeMuF
    { _typeMuExtension :: NoExt -- TODO
    , _expectKind      :: Kind -- TODO: Consider whether we'd prefer using a more intelligent kind-checking algorithm that could infer this.
    , _itselfType      :: typ
    }

  -- | Bind outer defined 'self'
  | TypeSelfBindingF
    { _typeSelfBindingExtension :: NoExt -- TODO
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
    { _sumTExtension :: SumTExtension phase
    , _sumTypes      :: NonEmpty typ
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
    { _productTExtension :: ProductTExtension phase
    , _productTypes      :: [typ]
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
    { _unionTExtension :: UnionTExtension phase
    , _unionTypes      :: Set.Set typ
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
    , _typeF            :: typ
    , _typeX            :: typ
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
    { _typeBindingExtension :: TypeBindingExtension phase
    , _typeBinding          :: TypeBindingFor phase
    }

  -- | Bind a type uniquely named by its content.
  --
  -- E.G. sha512/BGpbCZNFqE6aQE1
  --
  -- These hashes are currently non-cyclic and there is no way for a type to
  -- refer to itself meaning this does not introduce recursion.
  | TypeContentBindingF
    { _typeContentBindingExtension :: TypeContentBindingExtension phase
    , _typeContentBindingName      :: TypeContentBindingFor phase
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
  ,Eq (TypeContentBindingExtension phase)
  ,Eq (TypeExtension phase)
  ,Eq (TypeBindingFor phase)
  ,Eq (TypeContentBindingFor phase)
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
  ,Ord (TypeContentBindingExtension phase)
  ,Ord (TypeExtension phase)
  ,Ord (TypeBindingFor phase)
  ,Ord (TypeContentBindingFor phase)
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
  ,Show (TypeContentBindingExtension phase)
  ,Show (TypeExtension phase)
  ,Show (TypeBindingFor phase)
  ,Show (TypeContentBindingFor phase)
  ,Show typ
  )
  => Show (TypeF phase typ) where
    show = mconcat . \case
      NamedF ext t
        -> ["{Named ", show ext, " ", show t, "}"]

      ArrowF ext from to
        -> ["{Arrow ", show ext, " ", show from, " ", show to, "}"]

      SumTF ext ts
        -> ["{SumT ", show ext, " ", show ts, "}"]

      ProductTF ext ts
        -> ["{ProductT ", show ext, " ", show ts, "}"]

      UnionTF ext ts
        -> ["{UnionT ", show ext, " ", show ts, "}"]

      BigArrowF ext absKind t
        -> ["{ArrowT ", show ext, " ", show absKind, " ", show t, "}"]

      TypeLamF ext absKind t
        -> ["{TypeLam ", show ext, " ", show absKind, " ", show t, "}"]

      TypeAppF ext fT xT
        -> ["{TypeApp ", show ext, " ", show fT, " ", show xT, "}"]

      TypeBindingF ext b
        -> ["{TypeBinding ", show ext, " ", show b, "}"]

      TypeContentBindingF ext name
        -> ["{TypeContentBinding ", show ext, " ", show name, "}"]

      TypeMuF ext expectKy itself
        -> ["{TypeMu ", show ext, " ", show expectKy, " ", show itself, "}"]

      TypeSelfBindingF ext
        -> ["{TypeSelfBinding ", show ext, "}"]

      TypeExtensionF ext
        -> ["{TypeExtension ", show ext, "}"]

instance
  (Hashable (NamedExtension phase)
  ,Hashable (ArrowExtension phase)
  ,Hashable (SumTExtension phase)
  ,Hashable (ProductTExtension phase)
  ,Hashable (UnionTExtension phase)
  ,Hashable (BigArrowExtension phase)
  ,Hashable (TypeLamExtension phase)
  ,Hashable (TypeAppExtension phase)
  ,Hashable (TypeBindingExtension phase)
  ,Hashable (TypeContentBindingExtension phase)
  ,Hashable (TypeExtension phase)
  ,Hashable (TypeBindingFor phase)
  ,Hashable (TypeContentBindingFor phase)
  ,Hashable typ
  )
  => Hashable (TypeF phase typ) where
  toHashToken = \case
    -- Name deliberatly unhashed - we don't have type context
    -- This implies hashing is only useful when the Named extension
    -- contains a resolved name.
    NamedF ext _n
      -> HashTag "Type.Named" [toHashToken ext]

    ArrowF ext from to
      -> HashTag "Type.Arrow" [toHashToken ext, toHashToken from, toHashToken to]

    SumTF ext tys
      -> HashTag "Type.Arrow" [toHashToken ext, toHashToken tys]

    ProductTF ext tys
      -> HashTag "Type.Product" [toHashToken ext, toHashToken tys]

    UnionTF ext tys
      -> HashTag "Type.Union" [toHashToken ext, toHashToken tys]

    BigArrowF ext absKind t
      -> HashTag "Type.Arrow" [toHashToken ext, toHashToken absKind, toHashToken t]

    TypeLamF ext absKind t
      -> HashTag "Type.TypeLam" [toHashToken ext, toHashToken absKind, toHashToken t]

    TypeAppF ext fT xT
      -> HashTag "Type.TypeApp" [toHashToken ext, toHashToken fT, toHashToken xT]

    TypeBindingF ext b
      -> HashTag "Type.Binding" [toHashToken ext, toHashToken b]

    TypeMuF ext expectKy itselfTy
      -> HashTag "Type.Mu" [toHashToken ext, toHashToken expectKy, toHashToken itselfTy]

    TypeSelfBindingF ext
      -> HashTag "Type.Self" [toHashToken ext]

    TypeContentBindingF _ext c
      -> toHashToken c

    TypeExtensionF ext
      -> HashTag "Type.TypeExtension" [toHashToken ext]

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
type family TypeContentBindingExtension phase

-- The TypeExtension type family allows adding new constructors to the base Type
-- which depend upon the phase
type family TypeExtension phase

type family TypeBindingFor phase
type family TypeContentBindingFor phase

-- NamedF for phases where there is no extension to the constructor.
pattern Named :: NamedExtension phase ~ NoExt => TypeName -> TypeFor phase
pattern Named name <- FixPhase (NamedF _ name)
  where Named name =  FixPhase (NamedF noExt name)

pattern NamedExt :: NamedExtension phase -> TypeName -> TypeFor phase
pattern NamedExt ext name <- FixPhase (NamedF ext name)
  where NamedExt ext name =  FixPhase (NamedF ext name)

-- TypeMuF for phases where there is no extension to the constructor.
pattern TypeMu :: Kind -> TypeFor phase -> TypeFor phase
pattern TypeMu ky typ <- FixPhase (TypeMuF _ ky typ)
  where TypeMu ky typ =  FixPhase (TypeMuF noExt ky typ)

pattern TypeMuExt :: NoExt -> Kind -> TypeFor phase -> TypeFor phase
pattern TypeMuExt ext ky typ <- FixPhase (TypeMuF ext ky typ)
  where TypeMuExt ext ky typ =  FixPhase (TypeMuF ext ky typ)

-- TypeBindingSelf for phases where there is no extension to the constructor.
pattern TypeSelfBinding :: TypeFor phase
pattern TypeSelfBinding <- FixPhase (TypeSelfBindingF _)
  where TypeSelfBinding =  FixPhase (TypeSelfBindingF noExt)

pattern TypeSelfBindingExt :: NoExt -> TypeFor phase
pattern TypeSelfBindingExt ext <- FixPhase (TypeSelfBindingF ext)
  where TypeSelfBindingExt ext =  FixPhase (TypeSelfBindingF ext)

-- ArrowF for phases where there is no extension to the constructor.
pattern Arrow :: ArrowExtension phase ~ NoExt => TypeFor phase -> TypeFor phase -> TypeFor phase
pattern Arrow fromTy toTy <- FixPhase (ArrowF _ fromTy toTy)
  where Arrow fromTy toTy =  FixPhase (ArrowF noExt fromTy toTy)

pattern ArrowExt :: ArrowExtension phase -> TypeFor phase -> TypeFor phase -> TypeFor phase
pattern ArrowExt ext fromTy toTy <- FixPhase (ArrowF ext fromTy toTy)
  where ArrowExt ext fromTy toTy =  FixPhase (ArrowF ext fromTy toTy)

-- SumT for phases where there is no extension to the constructor.
pattern SumT :: SumTExtension phase ~ NoExt => NonEmpty (TypeFor phase) -> TypeFor phase
pattern SumT types <- FixPhase (SumTF _ types)
  where SumT types =  FixPhase (SumTF noExt types)

pattern SumTExt :: SumTExtension phase -> NonEmpty (TypeFor phase) -> TypeFor phase
pattern SumTExt ext types <- FixPhase (SumTF ext types)
  where SumTExt ext types =  FixPhase (SumTF ext types)

-- ProductT for phases where there is no extension to the constructor.
pattern ProductT :: ProductTExtension phase ~ NoExt => [TypeFor phase] -> TypeFor phase
pattern ProductT types <- FixPhase (ProductTF _ types)
  where ProductT types =  FixPhase (ProductTF noExt types)

-- ProductT for the empty product in phases where there is no extension to the constructor.
pattern EmptyProductT :: ProductTExtension phase ~ NoExt => TypeFor phase
pattern EmptyProductT <- FixPhase (ProductTF _ [])
  where EmptyProductT =  FixPhase (ProductTF noExt [])

pattern ProductTExt :: ProductTExtension phase -> [TypeFor phase] -> TypeFor phase
pattern ProductTExt ext types <- FixPhase (ProductTF ext types)
  where ProductTExt ext types =  FixPhase (ProductTF ext types)

pattern EmptyProductTExt :: ProductTExtension phase -> TypeFor phase
pattern EmptyProductTExt ext <- FixPhase (ProductTF ext [])
  where EmptyProductTExt ext =  FixPhase (ProductTF ext [])

-- UnionT for phases where there is no extension to the constructor.
pattern UnionT :: UnionTExtension phase ~ NoExt => Set.Set (TypeFor phase) -> TypeFor phase
pattern UnionT types <- FixPhase (UnionTF _ types)
  where UnionT types =  FixPhase (UnionTF noExt types)

pattern UnionTExt :: UnionTExtension phase -> Set.Set (TypeFor phase) -> TypeFor phase
pattern UnionTExt ext types <- FixPhase (UnionTF ext types)
  where UnionTExt ext types =  FixPhase (UnionTF ext types)

-- BigArrow for phases where there is no extension to the constructor.
pattern BigArrow :: BigArrowExtension phase ~ NoExt => Kind -> TypeFor phase -> TypeFor phase
pattern BigArrow kind t <- FixPhase (BigArrowF _ kind t)
  where BigArrow kind t =  FixPhase (BigArrowF noExt kind t)

pattern BigArrowExt :: BigArrowExtension phase -> Kind -> TypeFor phase -> TypeFor phase
pattern BigArrowExt ext kind t <- FixPhase (BigArrowF ext kind t)
  where BigArrowExt ext kind t =  FixPhase (BigArrowF ext kind t)

-- TypeLam for phases where there is no extension to the constructor.
pattern TypeLam :: TypeLamExtension phase ~ NoExt => Kind -> TypeFor phase -> TypeFor phase
pattern TypeLam absT t <- FixPhase (TypeLamF _ absT t)
  where TypeLam absT t =  FixPhase (TypeLamF noExt absT t)

pattern TypeLamExt :: TypeLamExtension phase -> Kind -> TypeFor phase -> TypeFor phase
pattern TypeLamExt ext absT t <- FixPhase (TypeLamF ext absT t)
  where TypeLamExt ext absT t =  FixPhase (TypeLamF ext absT t)

-- TypeApp for phases where there is no extension to the constructor.
pattern TypeApp :: TypeAppExtension phase ~ NoExt => TypeFor phase -> TypeFor phase -> TypeFor phase
pattern TypeApp fT xT <- FixPhase (TypeAppF _ fT xT)
  where TypeApp fT xT =  FixPhase (TypeAppF noExt fT xT)

pattern TypeAppExt :: TypeAppExtension phase -> TypeFor phase -> TypeFor phase -> TypeFor phase
pattern TypeAppExt ext fT xT <- FixPhase (TypeAppF ext fT xT)
  where TypeAppExt ext fT xT =  FixPhase (TypeAppF ext fT xT)

-- TypeBinding for phases where there is no extension to the constructor.
pattern TypeBinding :: TypeBindingExtension phase ~ NoExt => TypeBindingFor phase -> TypeFor phase
pattern TypeBinding tVar <- FixPhase (TypeBindingF _ tVar)
  where TypeBinding tVar =  FixPhase (TypeBindingF noExt tVar)

pattern TypeBindingExt :: TypeBindingExtension phase -> TypeBindingFor phase -> TypeFor phase
pattern TypeBindingExt ext tVar <- FixPhase (TypeBindingF ext tVar)
  where TypeBindingExt ext tVar =  FixPhase (TypeBindingF ext tVar)

-- TypeContentBinding for phases where there is no extension to the constructor.
pattern TypeContentBinding :: TypeContentBindingExtension phase ~ NoExt => TypeContentBindingFor phase -> TypeFor phase
pattern TypeContentBinding c <- FixPhase (TypeContentBindingF _ c)
  where TypeContentBinding c =  FixPhase (TypeContentBindingF noExt c)

pattern TypeContentBindingExt :: TypeContentBindingExtension phase -> TypeContentBindingFor phase -> TypeFor phase
pattern TypeContentBindingExt ext c <- FixPhase (TypeContentBindingF ext c)
  where TypeContentBindingExt ext c =  FixPhase (TypeContentBindingF ext c)

-- TypeExtensionF for phases where there is no extension to number of constructors.
pattern TypeExtension :: TypeExtension phase ~ NoExt => TypeFor phase
pattern TypeExtension <- FixPhase (TypeExtensionF _)
  where TypeExtension = FixPhase (TypeExtensionF noExt)

pattern TypeExtensionExt :: TypeExtension phase -> TypeFor phase
pattern TypeExtensionExt ext <- FixPhase (TypeExtensionF ext)
  where TypeExtensionExt ext = FixPhase (TypeExtensionF ext)

type instance NamedExtension DefaultPhase = NoExt
type instance ArrowExtension DefaultPhase = NoExt
type instance SumTExtension DefaultPhase = NoExt
type instance ProductTExtension DefaultPhase = NoExt
type instance UnionTExtension DefaultPhase = NoExt
type instance BigArrowExtension DefaultPhase = NoExt
type instance TypeLamExtension DefaultPhase = NoExt
type instance TypeAppExtension DefaultPhase = NoExt
type instance TypeBindingExtension DefaultPhase = NoExt
type instance TypeContentBindingExtension DefaultPhase = NoExt

-- TODO: Should _this_ be unit instead of noExt?
type instance TypeExtension DefaultPhase = NoExt

type instance TypeBindingFor DefaultPhase = TyVar
type instance TypeContentBindingFor DefaultPhase = ContentName

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
unarrowise = \case
  Arrow a b
    -> a : unarrowise b
  t -> [t]

-- TODO: Can likely use recursion schemes.

instance HasAbs Type where
  applyToAbs f = \case
    TypeLam k t
      -> TypeLam k (f t)

    TypeMu k itself
      -> TypeMu k (f itself)

    -- TODO:
    --  Nope(?)
    --  BigArrow ky ty -> BigArrow ky (f ty)

    t
      -> t

instance HasBinding Type TyVar where
  applyToBinding f = \case
    TypeBinding tb
      -> TypeBinding (f tb)

    t
      -> t

instance HasBinding Type ContentName where
  applyToBinding f (TypeContentBindingExt typ c) = TypeContentBindingExt typ (f c)
  applyToBinding _ e           = e

instance HasNonAbs Type where
  applyToNonAbs f = \case
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

    t -> t

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

      TypeContentBinding n
        -> TypeContentBinding n

      TypeMu ky itself
        -> TypeMu ky (instantiate' i instType itself)

      TypeSelfBinding
        -> TypeSelfBinding

      Named n
        ->  Named n

      _ -> error "Non-exhaustive pattern in type instantiation"

-- Forget all constructor extensions on a Type that uses TyVars for bindings in
-- order to produce a type in the default phase.
forgetTypeExtensions
  :: ( TypeBindingFor phase ~ TyVar
     , TypeContentBindingFor phase ~ ContentName
     )
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

  TypeContentBindingExt _ n
    -> TypeContentBinding n

  TypeMuExt _ ky itself
    -> TypeMu ky (forgetTypeExtensions itself)

  TypeSelfBindingExt _
    -> TypeSelfBinding

  TypeExtensionExt _
    -> TypeExtension

  _ -> error "Non-exhaustive pattern forgetting type extensions"

-- | Gather the Set of all TypeContentBinding names used within a Type
-- _without_ looking under any of the returned names themselves.
gatherTypeContentNames
  :: forall phase
   . TypeContentBindingFor phase ~ ContentName
  => TypeFor phase
  -> Set ContentName
gatherTypeContentNames = gatherTypeContentNames' Set.empty
  where
    gatherTypeContentNames' :: Set ContentName -> TypeFor phase -> Set ContentName
    gatherTypeContentNames' accNames = \case
      TypeContentBindingExt _ext c
        -> Set.insert c accNames

      NamedExt _ext _n
        -> accNames

      ArrowExt _ext from to
        -> gatherTypeContentNames' (gatherTypeContentNames' accNames from) to

      SumTExt _ext types
        -> foldr (flip gatherTypeContentNames') accNames types

      ProductTExt _ext types
        -> foldr (flip gatherTypeContentNames') accNames types

      UnionTExt _ext types
        -> foldr (flip gatherTypeContentNames') accNames types

      BigArrowExt _ext _abs toType
        -> gatherTypeContentNames' accNames toType

      TypeLamExt _ext _abs toType
        -> gatherTypeContentNames' accNames toType

      TypeAppExt _ext fType xType
        -> gatherTypeContentNames' (gatherTypeContentNames' accNames fType) xType

      TypeBindingExt _ext _binding
        -> accNames

      TypeSelfBindingExt _ext
        -> accNames

      TypeMuExt _ext _ky itself
        -> gatherTypeContentNames' accNames itself

      TypeExtensionExt _
        -> accNames

      _ -> error "Non-exhaustive pattern gathering type content names"

type instance ErrorType DefaultPhase = Type
type instance ErrorTypeBinding DefaultPhase = TyVar

