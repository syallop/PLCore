{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PL.Type where

import PL.Binds.Ix
import PL.Name
import PL.Kind
import PL.ExprLike

import Data.List
import qualified Data.Set as Set
import Data.Proxy

-- Describe properties of expressions
data Type tb

  -- | Some name
  = Named
    {_hasType :: TypeName
    }

  -- | A Function type between types
  | Arrow
    {_from :: Type tb
    ,_to   :: Type tb
    }

  -- | Ordered alternative types
  | SumT
    {_sumTypes :: [Type tb]
    }

  -- | Ordered product types
  | ProductT
    {_productTypes :: [Type tb]
    }

  -- | Set of union types
  | UnionT
    {_unionTypes :: Set.Set (Type tb)
    }

  -- Type of BigLambda
  -- Is this distinct from TypeLam??
  | BigArrow
    {_takeType :: Kind
    ,_type     :: Type tb
    }


  -- Type-level lambda abstraction
  | TypeLam
    {_takeType :: Kind
    ,_type     :: Type tb
    }

  -- Type-level application.
  | TypeApp
    {_f :: Type tb
    ,_x :: Type tb
    }

  | TypeBinding
    {_binding :: tb
    }
  deriving (Eq,Ord)

-- | Is a Type a simple named type
isType :: Type tb -> Bool
isType t = case t of
  Named _ -> True
  _       -> False

-- | Infix Arrow
(-->) :: Type tb -> Type tb -> Type tb
a --> b = Arrow a b

-- | Construct a simple named type
ty :: TypeName -> Type tb
ty b = Named b

instance Show tb => Show (Type tb) where
  show = showType

showType :: Show tb => Type tb -> String
showType t = case t of

    Arrow from to
      -> parens $ "^" ++ show from ++ " " ++ show to

    Named belongs
      -> show belongs

    SumT types
      -> parens $ ("+" ++) $ unwords $ map show types

    ProductT types
      -> parens $ ("*" ++) $ unwords $ map show types

    UnionT types
      -> parens $ ("U" ++) $ unwords $ map show $ Set.toList types

    BigArrow from to
      -> parens $ "^^" ++ show from ++ " " ++ show to

    TypeLam takeKind typ
      -> parens $ "\\" ++ show takeKind ++ " " ++ show typ

    TypeApp f x
      -> parens $ "@" ++ show f ++ " " ++ show x

    TypeBinding binding
      -> show binding

parensS :: Show a => a -> String
parensS = parens . show

parens :: String -> String
parens s = "(" ++ s ++ ")"

-- PARTIAL
-- [a]   ~> Type a
-- [a,b,c] ~> Arrow a (Arrow b c)
-- etc
arrowise :: [Type tb] -> Type tb
arrowise []        = error "Can't arrowise empty list of Types"
arrowise (t:[])    = t
arrowise (t:t':ts) = t --> arrowise (t':ts)

-- a           ~> [a]
-- a -> b -> c ~> [a,b,c]
-- etc
unarrowise :: Type tb -> [Type tb]
unarrowise (Arrow a b) = a : unarrowise b
unarrowise t           = [t]

instance HasAbs (Type tb) where
  applyToAbs f ty = case ty of
    TypeLam ky ty -> TypeLam ky (f ty)
--  Nope(?)
--  BigArrow ky ty -> BigArrow ky (f ty)
    ty            -> ty

instance HasBinding (Type tb) tb where
  applyToBinding f ty = case ty of
    TypeBinding tb -> TypeBinding (f tb)
    ty             -> ty

instance Ord tb => HasNonAbs (Type tb) where
  applyToNonAbs f ty = case ty of
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
instantiate instType inType = instantiate' 0 instType inType
  where
    {-instantiate' :: BindingIx tb => Int -> Type tb -> Type tb -> Type tb-}
    instantiate' i instType inType = case inType of
      Arrow from to
        -> Arrow (instantiate' i instType from) (instantiate' i instType to)

      SumT ts
        -> SumT $ map (instantiate' i instType) ts

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
        | bindDepth tb == i -> buryBy (Proxy :: Proxy tb) instType (BuryDepth i)
        | otherwise         -> TypeBinding tb

      Named n
        -> Named n

