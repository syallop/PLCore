module PL.Error where

import PL.Name
import PL.Type

data Error

  -- ^ Generic error
  = EMsg String

  -- No such name
  | ETypeNotDefined TypeName -- ^ No such type
  | ETermNotDefined TermName -- ^ No such term

  -- ^ Two typed things cannot be applied to each other
  | EAppMismatch Type Type --
  deriving (Ord,Eq)

instance Show Error where
    show = showError

showError :: Error -> String
showError e = let err m = "ERROR: " ++ m in err $ case e of
  EMsg msg
    -> show msg

  ETypeNotDefined name
    -> "Type named '" ++ show name ++ "' is not defined."

  ETermNotDefined name
    -> "Term named '" ++ show name ++ "' is not defined."

  EAppMismatch fTy xTy
    -> "Cannot apply expression typed: '" ++ show fTy ++ "' to expression typed: '" ++ show xTy ++ "'."

