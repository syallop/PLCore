{-# LANGUAGE
    FlexibleInstances
  , DeriveAnyClass
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  , OverloadedStrings
  #-}
{-|
Module      : PL.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Types inhabited by Expressions. Types are structural until explicitly Named
and can abstract and be applied much like Expressions.
-}
module PL.Type where

import PL.Binds.Ix
import PL.Name
import PL.Kind
import PL.ExprLike
import PL.FixType

import PLGrammar

import PLPrinter hiding (parens,between)
import PLPrinter.Doc

import Data.List
import qualified Data.Set as Set
import Data.Proxy
import Data.Monoid

-- | Types classify 'Expr'essions.
--
-- - 'tb' is the type of bindings which refer to some abstraction.
--   This could be a variable name "sometype".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to a type abstraction "0","1", etc.
type Type tb = FixType tb TypeF

-- | Types classify 'Expr'essions.
--
-- - 'tb' is the type of bindings which refer to some abstraction.
--   This could be a variable name "sometype".
--   Current uses of this AST use De Bruijn indexes, I.E. a natural number which
--   points a number of binders away to a type abstraction "0","1", etc.
--
-- - 'typ' is the recursive type of subexpressions. This is usually instantiated
--   using FixType such that it refers to itself.
--   This allows us to use recursion schemes.
--
--   Any examples used in constructor comments use completly arbitrary syntax.
data TypeF tb typ

  -- | Named type.
  --
  -- A name which refers to a type definition tracked in some external type context.
  = Named
    { _hasType :: TypeName
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
  | Arrow
    { _from :: typ
    , _to   :: typ
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
  | SumT
    { _sumTypes :: [typ]
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
  | ProductT
    { _productTypes :: [typ]
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
  | UnionT
    { _unionTypes :: Set.Set typ
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
  | BigArrow
    { _takeType :: Kind
    , _type     :: typ
    }

  -- | TypeLam is a lambda abstraction performed at the type level.
  --
  -- E.G.
  --
  -- \Kind. 0
  --
  -- TODO: Is being able to bind types at the expression and type level
  -- problematic?
  | TypeLam
    { _takeType :: Kind
    , _type     :: typ
    }

  -- | Type application.
  --
  -- Apply a type to another.
  --
  -- E.G.
  -- (\t::Kind -> t) Bool
  | TypeApp
    { _f :: typ
    , _x :: typ
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
  | TypeBinding
    { _binding :: tb
    }
  deriving (Eq,Ord,Show)

deriving instance (Document tb, Document typ) => Document (TypeF tb typ)

-- | Is a Type a simple named type
isType :: Type tb -> Bool
isType t = case unfixType t of
  Named _ -> True
  _       -> False

-- | Infix Arrow
(-->) :: Type tb -> Type tb -> Type tb
a --> b = fixType $ Arrow a b

-- | Construct a simple named type
ty :: TypeName -> Type tb
ty = fixType . Named

-- PARTIAL
-- [a]   ~> Type a
-- [a,b,c] ~> Arrow a (Arrow b c)
-- etc
arrowise :: [Type tb] -> Type tb
arrowise []        = error "Can't arrowise empty list of Types"
arrowise [t]       = t
arrowise (t:t':ts) = t --> arrowise (t':ts)

-- a           ~> [a]
-- a -> b -> c ~> [a,b,c]
-- etc
unarrowise :: Type tb -> [Type tb]
unarrowise t = case unfixType t of
  Arrow a b
    -> a : unarrowise b
  t -> [fixType t]

-- TODO: Can likely use recursion schemes.

instance HasAbs (Type tb) where
  applyToAbs f ty = fixType $ case unfixType ty of
    TypeLam ky ty -> TypeLam ky (f ty)
--  Nope(?)
--  BigArrow ky ty -> BigArrow ky (f ty)
    ty            -> ty

instance HasBinding (Type tb) tb where
  applyToBinding f ty = fixType $ case unfixType ty of
    TypeBinding tb -> TypeBinding (f tb)
    ty             -> ty

instance Ord tb => HasNonAbs (Type tb) where
  applyToNonAbs f ty = fixType $ case unfixType ty of
    Arrow from to
      -> Arrow (f from) (f to)

    SumT types
      -> SumT (map f types)

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
  :: forall tb
   . ( Ord tb
     , BindingIx tb
     )
  => Type tb
  -> Type tb
  -> Type tb
instantiate = instantiate' 0
  where
    instantiate' :: BindingIx tb => Int -> Type tb -> Type tb -> Type tb
    instantiate' i instType inType = case unfixType inType of
      Arrow from to
        -> fixType $ Arrow (instantiate' i instType from) (instantiate' i instType to)

      SumT ts
        -> fixType $ SumT $ map (instantiate' i instType) ts

      ProductT ts
        -> fixType $ ProductT $ map (instantiate' i instType) ts

      UnionT ts
        -> fixType $ UnionT $ Set.map (instantiate' i instType) ts

      BigArrow from to
        -> fixType $ BigArrow from (instantiate' i instType to)

      TypeLam from to
        -> fixType $ TypeLam from (instantiate' (i+1) instType to)

      TypeApp f x
        -> fixType $ TypeApp (instantiate' i instType f) (instantiate' i instType x)

      TypeBinding tb
        | bindDepth tb == i -> buryBy (Proxy :: Proxy tb) instType (BuryDepth i)
        | otherwise         -> fixType $ TypeBinding tb

      Named n
        -> fixType $ Named n

