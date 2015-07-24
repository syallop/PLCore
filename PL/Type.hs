module PL.Type where

import PL.Name

import Data.List
import qualified Data.Set as Set

-- | A type is either
data Type

  -- | Some name
  = Type
    {_hasType :: TypeName
    }

  -- | A Function type between types
  | Arrow
    {_from :: Type
    ,_to   :: Type
    }

  -- | Ordered alternative types
  | SumT
    {_sumTypes :: [Type]
    }

  -- | Ordered product types
  | ProdT
    {_prodTypes :: [Type]
    }


  | UnionT
    {_unionTypes :: Set.Set Type
    }
  deriving (Eq,Ord)

-- | Is a Type a simple named type
isType :: Type -> Bool
isType t = case t of
  Type _ -> True
  _      -> False

-- | Infix Arrow
(-->) :: Type -> Type -> Type
a --> b = Arrow a b

-- | Construct a simple named type
ty :: TypeName -> Type
ty b = Type b

instance Show Type where
  show t = case t of

    Arrow from to
      -> parens $ show from ++ " -> " ++ show to

    Type belongs
      -> show belongs

    SumT types
      -> parens $ intercalate "|" $ map show types

    ProdT types
      -> parens $ intercalate "*" $ map show types

    UnionT types
      -> parens $ "<" ++ (intercalate "|" $ map show $ Set.toList types) ++ ">"

parensS :: Show a => a -> String
parensS = parens . show

parens :: String -> String
parens s = "(" ++ s ++ ")"

-- PARTIAL
-- [a]   ~> Type a
-- [a,b,c] ~> Arrow a (Arrow b c)
-- etc
arrowise :: [Type] -> Type
arrowise (t:[])    = t
arrowise (t:t':ts) = t --> (arrowise (t':ts))

-- a           ~> [a]
-- a -> b -> c ~> [a,b,c]
-- etc
unarrowise :: Type -> [Type]
unarrowise (Arrow a b) = a : unarrowise b
unarrowise t           = [t]

