{-|
Module      : PL.Parser.Lispy.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Parser for PL.Type which consumes a lisp-like syntax.
-}
module PL.Parser.Lispy.Type where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import qualified Data.Text as Text

import PL.Parser
import PL.Parser.Lispy.Kind

import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type hiding (arrowise)

tyVar :: Parser TyVar
tyVar = mkTyVar <$> natural

-- A name is an uppercase followed by zero or more lower case characters
name :: Parser Text.Text
name = Text.cons <$> upper <*> takeWhile isLower

arrowise :: Type tb -> Type tb -> [Type tb] -> Type tb
arrowise from to []     = Arrow from to
arrowise from to (t:ts) = Arrow from (Arrow to (arrowise to t ts))

-- A named type is just a name which appears in the type position
namedTyp :: Ord tb => Parser (Type tb)
namedTyp = Named . TypeName <$> name

-- A '+' followed by zero or more types
sumTyp :: Ord tb => Parser tb -> Parser (Type tb)
sumTyp tb = SumT <$> (plus *> many (typ tb))

-- A '*' followed by zero or more types
productTyp :: Ord tb => Parser tb -> Parser (Type tb)
productTyp tb = ProductT <$> (star *> many (typ tb))

-- A 'U' followed by zero or more types
unionTyp :: Ord tb => Parser tb -> Parser (Type tb)
unionTyp tb = UnionT . Set.fromList <$> (union *> many (typ tb))

-- A '^' followed by two or more types
arrowTyp :: Ord tb => Parser tb -> Parser (Type tb)
arrowTyp tb = arrowise <$> (arrow *> typ tb) <*> typ tb <*> many (typ tb)

-- A "/\" followed by an abstracted kind, then a type
typeLamTyp :: Ord tb => Parser tb -> Parser (Type tb)
typeLamTyp tb = lamise <$> (bigLambda *> kindAbs) <*> many kindAbs <*> typ tb
  where
    -- chain lambda
    lamise :: Ord tb => Kind -> [Kind] -> Type tb -> Type tb
    lamise k0 []     t = TypeLam k0 t
    lamise k0 (k:ks) t = TypeLam k0 $ lamise k ks t

-- An "@@" followed by two or more types
typeAppTyp :: Ord tb => Parser tb -> Parser (Type tb)
typeAppTyp tb = appise <$> (bigAt *> typ tb) <*> typ tb <*> many (typ tb)
  where
    appise :: Type tb -> Type tb -> [Type tb] -> Type tb
    appise f x []     = TypeApp f x
    appise f x (y:ys) = appise (TypeApp f x) y ys

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Parser tb -> Parser (Type tb)
typeBindingTyp = fmap TypeBinding

-- A type is one of several variants, and may be nested in parenthesis
typ :: Ord tb => Parser tb -> Parser (Type tb)
typ tb
  = namedTyp
 <|> sumTyp tb
 <|> productTyp tb
 <|> unionTyp tb
 <|> arrowTyp tb
 <|> typeLamTyp tb
 <|> typeAppTyp tb
 {-<|> typeBindingTyp tb-}
 <|> betweenParens (typ tb)

