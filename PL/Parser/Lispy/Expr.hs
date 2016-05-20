{-# LANGUAGE OverloadedStrings #-}
module PL.Parser.Lispy.Expr where

import Control.Applicative
import qualified Data.Set as Set

import PL.Parser
import PL.Parser.Lispy.Type

import PL.Expr hiding (appise,lamise)
import PL.Type
import PL.Var

typeAbs = typ

-- A lambda followed by one or more type abstractions then an expression.
lamExpr = lamise <$> (lambda *> typeAbs) <*> many typeAbs <*> expr

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs -> Expr b abs
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e 


-- An '@' followed by two or more expressions
appExpr = appise <$> (at *> expr) <*> expr <*> many expr

-- Chain application
appise :: Expr b abs -> Expr b abs -> [Expr b abs] -> Expr b abs
appise f x []     = App f x
appise f x (y:ys) = appise (App f x) y ys

-- A var used as a binding
varBindingExpr = Binding <$> var

var = mkVar <$> natural 


-- A '+' followed by an index, an expression and two or more types
sumExpr = sumise <$> (plus *> natural) <*> expr <*> typ <*> typ <*> many typ

sumise :: Int -> Expr b abs -> Type -> Type -> [Type] -> Expr b abs
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr = productise <$> (star *> many expr)

productise :: [Expr b abs] -> Expr b abs
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr = unionise <$> (charIs 'U' *> typ) <*> expr <*> typ <*> typ <*> many typ

unionise :: Type -> Expr b abs -> Type -> Type -> [Type] -> Expr b abs
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg = bind
        <|> matchBinding
        <|> matchSum
        <|> matchProduct
        <|> matchUnion
        <|> betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum = MatchSum <$> (plus *> natural) <*> matchArg

-- A '*' followed by zero or more matchArgs
matchProduct = MatchProduct <$> (star *> many matchArg)

-- A 'U' followed by a type index and a matchArg
matchUnion = MatchUnion <$> (charIs 'U' *> typ) <*> matchArg

-- A var
matchBinding = MatchBinding <$> var

-- A '?'
bind = question *> pure Bind

-- A matchArg pattern, then an expression
caseBranch = caseBranch' <|> betweenParens caseBranch'
  where caseBranch' = CaseBranch <$> (charIs '|' *> matchArg) <*> expr

-- One or many caseBranch
someCaseBranches = SomeCaseBranches <$> caseBranch <*> many caseBranch

-- An expr
defaultOnly = DefaultOnly <$> expr

-- someCaseBranches then a possible expr
caseBranches = CaseBranches <$> someCaseBranches <*> ((Just <$> expr) <|> pure Nothing)

-- either caseBranches or a defaultOnly expr
possibleCaseBranches = caseBranches <|> defaultOnly

-- "CASE", then ann expr then possibleCaseBranches
caseExpr = Case <$> (textIs "CASE" *> expr) <*> possibleCaseBranches

expr =  lamExpr
    <|> appExpr
    <|> sumExpr
    <|> productExpr
    <|> unionExpr
    <|> caseExpr
    <|> varBindingExpr
    <|> betweenParens expr

