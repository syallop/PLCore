{-# LANGUAGE FlexibleInstances
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

type Type tb = FixType tb TypeF

-- Describe properties of expressions
data TypeF tb typ

  -- | Some name
  = Named
    {_hasType :: TypeName
    }

  -- | A Function type between types
  | Arrow
    {_from :: typ
    ,_to   :: typ
    }

  -- | Ordered alternative types
  | SumT
    {_sumTypes :: [typ]
    }

  -- | Ordered product types
  | ProductT
    {_productTypes :: [typ]
    }

  -- | Set of union types
  | UnionT
    {_unionTypes :: Set.Set typ
    }

  -- Type of BigLambda
  -- Is this distinct from TypeLam??
  | BigArrow
    {_takeType :: Kind
    ,_type     :: typ
    }


  -- Type-level lambda abstraction
  | TypeLam
    {_takeType :: Kind
    ,_type     :: typ
    }

  -- Type-level application.
  | TypeApp
    {_f :: typ
    ,_x :: typ
    }

  | TypeBinding
    {_binding :: tb
    }
  deriving (Eq,Ord,Show)

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

instance Document tb => Document (Type tb) where
  document t = case unfixType t of
    Arrow from to
      -> char '^' <> parens (document from) <> parens (document to)

    Named belongs
      -> document belongs

    SumT tys
      -> text "+" <> (mconcat . map (parens . document) $ tys)

    ProductT tys
      -> text "*" <> (mconcat . map (parens . document) $ tys)

    UnionT tys
      -> text "U" <> (mconcat . map (parens . document) . Set.toList $ tys)

    BigArrow from to
      -> text "^^" <> parens (document from) <> parens (document to)

    TypeLam takeKy ty
      -> text "\\" <> parens (document takeKy) <> parens (document ty)

    TypeApp f x
      -> text "@" <> parens (document f) <> parens (document x)

    TypeBinding b
      -> document b


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

instantiate :: forall tb. (Ord tb,BindingIx tb) => Type tb -> Type tb -> Type tb
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

