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
lamExpr :: Ord tb => Parser b -> Parser abs -> Parser tb
        -> Parser (Expr b abs tb)
lamExpr eb abs tb = lamise <$> (lambda *> abs) <*> many abs <*> expr eb abs tb

-- Chain lambda
lamise :: abs -> [abs] -> Expr b abs tb -> Expr b abs tb
lamise a0 []     e = Lam a0 e
lamise a0 (a:as) e = Lam a0 $ lamise a as e


-- An '@' followed by two or more expressions
appExpr :: Ord tb => Parser b -> Parser abs -> Parser tb
        -> Parser (Expr b abs tb)
appExpr eb abs tb = appise <$> (at *> (expr eb abs tb)) <*> expr eb abs tb <*> many (expr eb abs tb)

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
sumExpr :: Ord tb => Parser b -> Parser abs -> Parser tb
        -> Parser (Expr b abs tb)
sumExpr eb abs tb = sumise <$> (plus *> natural) <*> expr eb abs tb <*> typ tb <*> typ tb <*> many (typ tb)

sumise :: Int -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
sumise ix e t0 t1 ts = Sum e ix (t0:t1:ts)


-- A '*' followed by zero or more expressions
productExpr :: Ord tb => Parser b -> Parser abs -> Parser tb
            -> Parser (Expr b abs tb)
productExpr eb abs tb = productise <$> (star *> many (expr eb abs tb))

productise :: [Expr b abs tb] -> Expr b abs tb
productise = Product


-- A 'U' followed by its type index, the expression and two or more types
unionExpr :: Ord tb => Parser b -> Parser abs -> Parser tb
          -> Parser (Expr b abs tb)
unionExpr eb abs tb = unionise <$> (union *> (typ tb)) <*> expr eb abs tb <*> typ tb <*> typ tb <*> many (typ tb)

unionise :: Ord tb => Type tb -> Expr b abs tb -> Type tb -> Type tb -> [Type tb] -> Expr b abs tb
unionise tIx e t0 t1 ts = Union e tIx (Set.fromList $ t0:t1:ts)


matchArg :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (MatchArg b tb)
matchArg eb abs tb
  = bind
 <|> matchBinding eb
 <|> matchSum eb abs tb
 <|> matchProduct eb abs tb
 <|> matchUnion eb abs tb
 <|> betweenParens (matchArg eb abs tb)

-- A '+' followed by an index and a matchArg
matchSum :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (MatchArg b tb)
matchSum eb abs tb = MatchSum <$> (plus *> natural) <*> matchArg eb abs tb

-- A '*' followed by zero or more matchArgs
matchProduct :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (MatchArg b tb)
matchProduct eb abs tb = MatchProduct <$> (star *> many (matchArg eb abs tb))

-- A 'U' followed by a type index and a matchArg
matchUnion :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (MatchArg b tb)
matchUnion eb abs tb = MatchUnion <$> (union *> (typ tb)) <*> matchArg eb abs tb

-- A var
matchBinding :: Parser b -> Parser (MatchArg b tb)
matchBinding eb = MatchBinding <$> eb

-- A '?'
bind :: Parser (MatchArg b tb)
bind = question *> pure Bind

-- A matchArg pattern, then an expression
caseBranch :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (CaseBranch b abs tb)
caseBranch eb abs tb = caseBranch' <|> betweenParens caseBranch'
  where caseBranch' = CaseBranch <$> (charIs '|' *> (matchArg eb abs tb)) <*> expr eb abs tb

-- One or many caseBranch
someCaseBranches :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (SomeCaseBranches b abs tb)
someCaseBranches eb abs tb = SomeCaseBranches <$> caseBranch eb abs tb <*> many (caseBranch eb abs tb)

-- An expr
defaultOnly :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (PossibleCaseBranches b abs tb)
defaultOnly eb abs tb = DefaultOnly <$> expr eb abs tb

-- someCaseBranches then a possible expr
caseBranches :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (PossibleCaseBranches b abs tb)
caseBranches eb abs tb = CaseBranches <$> someCaseBranches eb abs tb <*> ((Just <$> expr eb abs tb) <|> pure Nothing)

-- either caseBranches or a defaultOnly expr
possibleCaseBranches :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (PossibleCaseBranches b abs tb)
possibleCaseBranches eb abs tb = caseBranches eb abs tb <|> defaultOnly eb abs tb

-- "CASE", then ann expr then possibleCaseBranches
caseExpr :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (Expr b abs tb)
caseExpr eb abs tb = Case <$> (textIs "CASE" *> (expr eb abs tb)) <*> possibleCaseBranches eb abs tb

expr :: Ord tb => Parser b -> Parser abs -> Parser tb -> Parser (Expr b abs tb)
expr eb abs tb
  = lamExpr eb abs tb
 <|> appExpr eb abs tb
 <|> sumExpr eb abs tb
 <|> productExpr eb abs tb
 <|> unionExpr eb abs tb
 <|> caseExpr eb abs tb
 <|> bindingExpr eb
 <|> betweenParens (expr eb abs tb)

