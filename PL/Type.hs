module PL.Type where

import PL.Name
import PL.Kind

import Data.List
import qualified Data.Set as Set

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
      -> parens $ ("+" ++) $ intercalate " " $ map show types

    ProductT types
      -> parens $ ("*" ++) $ intercalate " " $ map show types

    UnionT types
      -> parens $ ("U" ++) $ intercalate " " $ map show $ Set.toList types

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

