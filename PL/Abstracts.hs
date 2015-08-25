module PL.Abstracts where

import PL.Type

class (Show abs,Eq abs) => Abstracts abs where
  absTy :: abs -> Type

instance Abstracts Type where
  absTy = id

