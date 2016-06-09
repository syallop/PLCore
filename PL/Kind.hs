module PL.Kind where

-- Describe properties of types
data Kind

  -- | Simple kind
  = Kind

  -- | Kind of type lambdas
  | KindArrow
    {_from :: Kind
    ,_to   :: Kind
    }
  deriving (Eq,Ord,Show)
