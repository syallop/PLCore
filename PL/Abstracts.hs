module PL.Abstracts where

import PL.Type

-- A type used in a lambda abstraction,
-- I.E. \ABS -> ...
class (Show abs,Eq abs) => Abstracts abs where
  absTy :: abs -> Type

-- The simplest abstraction is a type with no additional data (like E.G. a variable name)
instance Abstracts Type where
  absTy = id

