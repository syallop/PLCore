{-# LANGUAGE ScopedTypeVariables, ImplicitParams #-}
{-|
Module      : PL.Grammar.Lispy.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Type with a lisp-like syntax.
-}
module PL.Grammar.Lispy.Type where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import PLGrammar
import PLGrammar.Iso

import PL.Grammar.Lispy.TypeIso
import PL.Grammar.Lispy.Kind

import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type hiding (arrowise)

tyVar :: Grammar TyVar
tyVar = charIs '?' */ (tyVarIso \$/ natural)

-- A name is an uppercase followed by zero or more lower case characters
name :: Grammar Text.Text
name = nameIso \$/ upper \*/ longestMatching isLower

typeName :: Grammar TypeName
typeName = typeNameIso \$/ name

-- A named type is just a name which appears in the type position
namedTyp :: (Ord tb,Show tb) => Grammar (Type tb)
namedTyp = namedIso \$/ typeName

-- A plus followed by zero or more types
sumTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
sumTyp tb = plus */ (sumTIso \$/ grammarMany (spaceRequired */ typ tb))

-- A star followed by zero or more types
productTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
productTyp tb = star */ (productTIso \$/ grammarMany (spaceRequired */ typ tb))

-- A union followed by zero or more types
unionTyp :: forall tb. (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
unionTyp tb = union */ (unionTIso \$/ (setIso \$/ grammarMany (spaceRequired */ typ tb)))

-- An arrow followed by two or more types
arrowTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
arrowTyp tb = arrow */ (arrowIso \$/ typ tb \*/ (spaceRequired */ typ tb))

-- A type-lambda followed by an abstracted kind, then a type
typeLamTyp :: forall tb. (Ord tb,Show tb) => Grammar tb -> Grammar (Type tb)
typeLamTyp tb = bigLambda */ (typeLamIso \$/ kindAbs \*/ (spaceRequired */ typ tb))

-- An type-app followed by two or more types
typeAppTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typeAppTyp tb = bigAt */ (typeAppIso \$/ typ tb \*/ (spaceRequired */ typ tb))

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Show tb => Grammar tb -> Grammar (Type tb)
typeBindingTyp gtb = typeBindingIso \$/ gtb

typI :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
typI = let tb = ?tb in typ tb

-- A type is one of several variants, and may be nested in parenthesis
typ :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typ tb = alternatives
  [ typeLamTyp tb
  , typeAppTyp tb
  , arrowTyp tb
  , sumTyp tb
  , productTyp tb
  , unionTyp tb
  , namedTyp
  , typeBindingTyp tb
  , try $ betweenParens $ typ tb
  ]

