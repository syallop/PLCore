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

arrowise :: Type -> Type -> [Type] -> Type
arrowise from to []     = Arrow from to
arrowise from to (t:ts) = Arrow from (Arrow to (arrowise to t ts))

-- A named type is just a name which appears in the type position
namedTyp = Named . TypeName <$> name

-- A '+' followed by zero or more types
sumTyp = SumT <$> (plus *> many typ)

-- A '*' followed by zero or more types
productTyp = ProductT <$> (star *> many typ)

-- A 'U' followed by zero or more types
unionTyp = UnionT . Set.fromList <$> (union *> many typ)

-- A '^' followed by two or more types
arrowTyp = arrowise <$> (arrow *> typ) <*> typ <*> many typ

-- A type is one of several variants, and may be nested in parenthesis
typ :: Parser Type
typ =  namedTyp
   <|> sumTyp
   <|> productTyp
   <|> unionTyp
   <|> arrowTyp
   <|> betweenParens typ

