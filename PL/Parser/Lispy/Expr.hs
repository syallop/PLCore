{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
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
lamExpr :: (Ord tb,Implicits b abs tb) => Parser (Expr b abs tb)
lamExpr = lamise <$> (lambda *> ?abs) <*> many ?abs <*> exprI

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- An '@' followed by two or more expressions
appExpr :: (Ord tb, Implicits b abs tb) => Parser (Expr b abs tb)
appExpr = appise <$> (at *> exprI) <*> exprI <*> many exprI

-- Chain application
appise :: Expr b abs tb -> Expr b abs tb -> [Expr b abs tb] -> Expr b abs tb
appise f x []     = App f x
appise f x (y:ys) = appise (App f x) y ys

bindingExpr :: Parser b -> Parser (Expr b abs tb)
bindingExpr eb = Binding <$> eb

-- A var used as a binding
varBindingExpr :: Parser (Expr Var abs tb)
varBindingExpr = Binding <$> var

var :: Parser Var
var = mkVar <$> natural


-- A '+' followed by an index, an expression and two or more types
sumExpr :: (Ord tb,Implicits b abs tb) => Parser (Expr b abs tb)
sumExpr = sumise <$> (plus *> natural) <*> exprI <*> typ ?tb <*> typ ?tb <*> many (typ ?tb)

sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: (Ord tb,Implicits b abs tb) => Parser (Expr b abs tb)
productExpr = productise <$> (star *> many exprI)

productise :: [Expr b abs tb] -> Expr b abs tb
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: (Ord tb,Implicits b abs tb) => Parser (Expr b abs tb)
unionExpr = unionise <$> (union *> (typ ?tb)) <*> exprI <*> typ ?tb <*> typ ?tb <*> many (typ ?tb)

unionise :: (Ord tb, Implicits b abs tb) => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: (Ord tb,Implicits b abs tb) => Parser (MatchArg b tb)
matchArg
  = bind
 <|> matchBinding ?eb
 <|> matchSum
 <|> matchProduct
 <|> matchUnion
 <|> betweenParens matchArg

-- A '+' followed by an index and a matchArg
matchSum :: (Ord tb, Implicits b abs tb) => Parser (MatchArg b tb)
matchSum = MatchSum <$> (plus *> natural) <*> matchArg

-- A '*' followed by zero or more matchArgs
matchProduct :: (Ord tb, Implicits b abs tb) => Parser (MatchArg b tb)
matchProduct = MatchProduct <$> (star *> many matchArg)

-- A 'U' followed by a type index and a matchArg
matchUnion :: (Ord tb,Implicits b abs tb) => Parser (MatchArg b tb)
matchUnion = MatchUnion <$> (union *> (typ ?tb)) <*> matchArg

-- A var
matchBinding :: Parser b -> Parser (MatchArg b tb)
matchBinding eb = MatchBinding <$> eb

-- A '?'
bind :: Parser (MatchArg b tb)
bind = question *> pure Bind

-- A matchArg pattern, then an expression
caseBranch :: (Ord tb,Implicits b abs tb) => Parser (CaseBranch b abs tb)
caseBranch = caseBranch' <|> betweenParens caseBranch'
  where caseBranch' = CaseBranch <$> (charIs '|' *> matchArg) <*> exprI

-- One or many caseBranch
someCaseBranches :: (Ord tb,Implicits b abs tb) => Parser (SomeCaseBranches b abs tb)
someCaseBranches = SomeCaseBranches <$> caseBranch <*> many caseBranch

-- An expr
defaultOnly :: (Ord tb, Implicits b abs tb) => Parser (PossibleCaseBranches b abs tb)
defaultOnly = DefaultOnly <$> exprI

-- someCaseBranches then a possible expr
caseBranches :: (Ord tb, Implicits b abs tb) => Parser (PossibleCaseBranches b abs tb)
caseBranches = CaseBranches <$> someCaseBranches <*> ((Just <$> exprI) <|> pure Nothing)

-- either caseBranches or a defaultOnly expr
possibleCaseBranches :: (Ord tb, Implicits b abs tb) => Parser (PossibleCaseBranches b abs tb)
possibleCaseBranches = caseBranches <|> defaultOnly

-- "CASE", then ann expr then possibleCaseBranches
caseExpr :: (Ord tb,Implicits b abs tb) => Parser (Expr b abs tb)
caseExpr = Case <$> (textIs "CASE" *> exprI) <*> possibleCaseBranches


-- Implicitly bind Parsers for expression bindings, abstractions and type bindings
type Implicits b abs tb = (?eb :: Parser b,?abs :: Parser abs,?tb :: Parser tb)

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI :: (Ord tb, Implicits b abs tb) => Parser (Expr b abs tb)
exprI
  = lamExpr
 <|> appExpr
 <|> sumExpr
 <|> productExpr
 <|> unionExpr
 <|> caseExpr
 <|> bindingExpr ?eb
 <|> betweenParens exprI

-- Parse an expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. Type)
-- - Type bindings          (E.G. Var)
expr :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI
