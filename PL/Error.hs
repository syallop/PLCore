module PL.Error where

import PL.Name
import PL.Type
import PL.Kind

data Error tb

  -- ^ Generic error
  = EMsg String

  -- No such name
  | ETypeNotDefined TypeName -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- ^ Two typed things cannot be applied to each other
  | EAppMismatch (Type tb) (Type tb) --

  -- ^ Something with type cannot be big-applied to something with kind
  | EBigAppMismatch (Type tb) Kind

  -- ^ Something with kind cannot be type-applied to something with kind
  | ETypeAppMismatch Kind Kind
  deriving (Ord,Eq)

instance Show tb => Show (Error tb) where
    show = showError

showError :: Show tb => Error tb -> String
showError e = let err m = "ERROR: " ++ m in err $ case e of
  EMsg msg
    -> show msg

  ETypeNotDefined name
    -> "Type named '" ++ show name ++ "' is not defined."

  ETermNotDefined name
    -> "Term named '" ++ show name ++ "' is not defined."

  EAppMismatch fTy xTy
    -> "Cannot apply expression typed: '" ++ show fTy ++ "' to expression typed: '" ++ show xTy ++ "'."

  EBigAppMismatch fTy xKy
    -> "Cannot apply expression typed: '" ++ show fTy ++ "' to type kinded: '" ++ show xKy ++ "'."

  ETypeAppMismatch fKy xKy
    -> "Cannot apply type kinded: '" ++ show fKy ++ "' to type kinded: '" ++ show xKy ++ "'."

