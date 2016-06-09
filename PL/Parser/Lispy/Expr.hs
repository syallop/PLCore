{-# LANGUAGE OverloadedStrings #-}
module PL.Parser.Lispy.Expr where

import Control.Applicative
import qualified Data.Set as Set

import PL.Parser
import PL.Parser.Lispy.Type

import PL.Expr hiding (appise,lamise)
import PL.Type
import PL.Var

typeAbs :: Ord tb => Parser (Type tb)
typeAbs = typ

-- A lambda followed by one or more type abstractions then an expression.
lamExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
lamExpr = lamise <$> (lambda *> typeAbs) <*> many typeAbs <*> expr

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- An '@' followed by two or more expressions
appExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
appExpr = appise <$> (at *> expr) <*> expr <*> many expr

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
sumExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
sumExpr = sumise <$> (plus *> natural) <*> expr <*> typ <*> typ <*> many typ

sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
productExpr = productise <$> (star *> many expr)

productise :: [Expr b abs tb] -> Expr b abs tb
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
unionExpr = unionise <$> (union *> typ) <*> expr <*> typ <*> typ <*> many typ

unionise :: Ord tb => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: Ord tb => Parser (MatchArg Var tb)
matchArg = bind
        <|> matchBinding
        <|> matchSum
        <|> matchProduct
        <|> matchUnion
        <|> betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum :: Ord tb => Parser (MatchArg Var tb)
matchSum = MatchSum <$> (plus *> natural) <*> matchArg

-- A '*' followed by zero or more matchArgs
matchProduct :: Ord tb => Parser (MatchArg Var tb)
matchProduct = MatchProduct <$> (star *> many matchArg)

-- A 'U' followed by a type index and a matchArg
matchUnion :: Ord tb => Parser (MatchArg Var tb)
matchUnion = MatchUnion <$> (union *> typ) <*> matchArg

-- A var
matchBinding :: Parser (MatchArg Var tb)
matchBinding = MatchBinding <$> var

-- A '?'
bind :: Parser (MatchArg Var tb)
bind = question *> pure Bind

-- A matchArg pattern, then an expression
caseBranch :: Ord tb => Parser (CaseBranch Var (Type tb) tb)
caseBranch = caseBranch' <|> betweenParens caseBranch'
  where caseBranch' = CaseBranch <$> (charIs '|' *> matchArg) <*> expr

-- One or many caseBranch
someCaseBranches :: Ord tb => Parser (SomeCaseBranches Var (Type tb) tb)
someCaseBranches = SomeCaseBranches <$> caseBranch <*> many caseBranch

-- An expr
defaultOnly :: Ord tb => Parser (PossibleCaseBranches Var (Type tb) tb)
defaultOnly = DefaultOnly <$> expr

-- someCaseBranches then a possible expr
caseBranches :: Ord tb => Parser (PossibleCaseBranches Var (Type tb) tb)
caseBranches = CaseBranches <$> someCaseBranches <*> ((Just <$> expr) <|> pure Nothing)

-- either caseBranches or a defaultOnly expr
possibleCaseBranches :: Ord tb => Parser (PossibleCaseBranches Var (Type tb) tb)
possibleCaseBranches = caseBranches <|> defaultOnly

-- "CASE", then ann expr then possibleCaseBranches
caseExpr :: Ord tb => Parser (Expr Var (Type tb) tb)
caseExpr = Case <$> (textIs "CASE" *> expr) <*> possibleCaseBranches

expr :: Ord tb => Parser (Expr Var (Type tb) tb)
expr =  lamExpr
    <|> appExpr
    <|> sumExpr
    <|> productExpr
    <|> unionExpr
    <|> caseExpr
    <|> varBindingExpr
    <|> betweenParens expr

