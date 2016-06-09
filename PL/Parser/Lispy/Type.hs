module PL.Parser.Lispy.Type where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import qualified Data.Text as Text
import qualified Data.Set as Set

import PL.Parser

import PL.Type hiding (arrowise)
import PL.Name

-- A name is an uppercase followed by zero or more lower case characters
name = Text.cons <$> upper <*> takeWhile isLower

arrowise :: Type tb -> Type tb -> [Type tb] -> Type tb
arrowise from to []     = Arrow from to
arrowise from to (t:ts) = Arrow from (Arrow to (arrowise to t ts))

-- A named type is just a name which appears in the type position
namedTyp :: Ord tb => Parser (Type tb)
namedTyp = Named . TypeName <$> name

-- A '+' followed by zero or more types
sumTyp :: Ord tb => Parser (Type tb)
sumTyp = SumT <$> (plus *> many typ)

-- A '*' followed by zero or more types
productTyp :: Ord tb => Parser (Type tb)
productTyp = ProductT <$> (star *> many typ)

-- A 'U' followed by zero or more types
unionTyp :: Ord tb => Parser (Type tb)
unionTyp = UnionT . Set.fromList <$> (union *> many typ)

-- A '^' followed by two or more types
arrowTyp :: Ord tb => Parser (Type tb)
arrowTyp = arrowise <$> (arrow *> typ) <*> typ <*> many typ

-- A type is one of several variants, and may be nested in parenthesis
typ :: Ord tb => Parser (Type tb)
typ =  namedTyp
   <|> sumTyp
   <|> productTyp
   <|> unionTyp
   <|> arrowTyp
   <|> betweenParens typ

