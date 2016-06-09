{-# LANGUAGE OverloadedStrings #-}
module PL.Parser.Lispy.Expr where

import Control.Applicative
import qualified Data.Set as Set

import PL.Parser
import PL.Parser.Lispy.Type

import PL.Expr hiding (appise,lamise)
import PL.Type
import PL.Var

typeAbs :: Ord tb => Parser tb -> Parser (Type tb)
typeAbs tb = typ tb

-- A lambda followed by one or more type abstractions then an expression.
lamExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
lamExpr tb = lamise <$> (lambda *> (typeAbs tb)) <*> many (typeAbs tb) <*> expr tb

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- An '@' followed by two or more expressions
appExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
appExpr tb = appise <$> (at *> (expr tb)) <*> expr tb <*> many (expr tb)

-- Chain application
appise :: Expr b abs tb -> Expr b abs tb -> [Expr b abs tb] -> Expr b abs tb
appise f x []     = App f x
appise f x (y:ys) = appise (App f x) y ys

-- A var used as a binding
varBindingExpr :: Parser (Expr Var (Type tb) tb)
varBindingExpr = Binding <$> var

var :: Parser Var
var = mkVar <$> natural


-- A '+' followed by an index, an expression and two or more types
sumExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
sumExpr tb = sumise <$> (plus *> natural) <*> expr tb <*> typ tb <*> typ tb <*> many (typ tb)

sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
productExpr tb = productise <$> (star *> many (expr tb))

productise :: [Expr b abs tb] -> Expr b abs tb
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
unionExpr tb = unionise <$> (union *> (typ tb)) <*> expr tb <*> typ tb <*> typ tb <*> many (typ tb)

unionise :: Ord tb => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: Ord tb => Parser tb -> Parser (MatchArg Var tb)
matchArg tb
  = bind
 <|> matchBinding
 <|> matchSum tb
 <|> matchProduct tb
 <|> matchUnion tb
 <|> betweenParens (matchArg tb)

-- A '+' followed by an index and a matchArg
matchSum :: Ord tb => Parser tb -> Parser (MatchArg Var tb)
matchSum tb = MatchSum <$> (plus *> natural) <*> matchArg tb

-- A '*' followed by zero or more matchArgs
matchProduct :: Ord tb => Parser tb -> Parser (MatchArg Var tb)
matchProduct tb = MatchProduct <$> (star *> many (matchArg tb))

-- A 'U' followed by a type index and a matchArg
matchUnion :: Ord tb => Parser tb -> Parser (MatchArg Var tb)
matchUnion tb = MatchUnion <$> (union *> (typ tb)) <*> matchArg tb

-- A var
matchBinding :: Parser (MatchArg Var tb)
matchBinding = MatchBinding <$> var

-- A '?'
bind :: Parser (MatchArg Var tb)
bind = question *> pure Bind

-- A matchArg pattern, then an expression
caseBranch :: Ord tb => Parser tb -> Parser (CaseBranch Var (Type tb) tb)
caseBranch tb = caseBranch' <|> betweenParens caseBranch'
  where caseBranch' = CaseBranch <$> (charIs '|' *> (matchArg tb)) <*> expr tb

-- One or many caseBranch
someCaseBranches :: Ord tb => Parser tb -> Parser (SomeCaseBranches Var (Type tb) tb)
someCaseBranches tb = SomeCaseBranches <$> caseBranch tb <*> many (caseBranch tb)

-- An expr
defaultOnly :: Ord tb => Parser tb -> Parser (PossibleCaseBranches Var (Type tb) tb)
defaultOnly tb = DefaultOnly <$> expr tb

-- someCaseBranches then a possible expr
caseBranches :: Ord tb => Parser tb -> Parser (PossibleCaseBranches Var (Type tb) tb)
caseBranches tb = CaseBranches <$> someCaseBranches tb <*> ((Just <$> expr tb) <|> pure Nothing)

-- either caseBranches or a defaultOnly expr
possibleCaseBranches :: Ord tb => Parser tb -> Parser (PossibleCaseBranches Var (Type tb) tb)
possibleCaseBranches tb = caseBranches tb <|> defaultOnly tb

-- "CASE", then ann expr then possibleCaseBranches
caseExpr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
caseExpr tb = Case <$> (textIs "CASE" *> (expr tb)) <*> possibleCaseBranches tb

expr :: Ord tb => Parser tb -> Parser (Expr Var (Type tb) tb)
expr tb
  = lamExpr tb
 <|> appExpr tb
 <|> sumExpr tb
 <|> productExpr tb
 <|> unionExpr tb
 <|> caseExpr tb
 <|> varBindingExpr
 <|> betweenParens (expr tb)

