{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module PL.Parser.Lispy where

import Prelude hiding (takeWhile,exp)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char
import qualified Data.Set as Set

import PL.Parser
import PL.Expr
import PL.Var
import PL.Name
import PL.Type

import PL.Binds

type ExprV = Expr Var Type

-- Utils
consP :: Parser a -> Parser [a] -> Parser [a]
consP = liftA2 (:)

arrowiseP :: Parser Type -> Parser [Type] -> Parser Type
arrowiseP = liftA2 (\t ts -> arrowise (t:ts))

appiseP :: Parser ExprV -> Parser [ExprV] -> Parser ExprV
appiseP = liftA2 (\e es -> appise (e:es))

lamiseP :: Parser [Type] -> Parser ExprV -> Parser ExprV
lamiseP = liftA2 lamise

grouped :: Parser a -> Parser a
grouped p = lparen *> p <* rparen


-- Primitive things
name     = Text.cons <$> upper <*> (takeWhile isLower)
termName = charIs '#' >> TermName <$> name
typeName = TypeName <$> name
var      = mkVar <$> natural

-- Type signatures
typeSig     = typeNameSig
           <|> (alternatives $ map betweenParens
            [sumSig
            ,productSig
            ,unionSig
            ,arrowSig
            ,typeNameSig
            ])
typeNameSig = Type <$> typeName
sumSig      = SumT                    <$> consP typeSig (some (plus  *> typeSig))
productSig  = ProductT                <$> consP typeSig (some (star  *> typeSig))
unionSig    = (UnionT . Set.fromList) <$> consP typeSig (some (comma *> typeSig))
arrowSig    = arrowiseP                         typeSig (some (up    *> typeSig))

-- Expressions
expr   =  (alternatives [varE,termE,lamEs,caseE,sumE,prodE,unionE])
      <|> (alternatives $ map betweenParens
       [varE
       ,termE
       ,lamEs
       ,appEs
       ,caseE
       ,sumE
       ,prodE
       ,unionE
       ])

varE   = Binding <$> var
termE  = Term <$> termName                                   -- {TermName}
lamEs  = lamiseP (lambda *> some typeSig) expr               -- \{Type1} [{Type} ]{Expr}
appEs  = appiseP expr (some expr)                            -- {Expr}1>[ {Expr}]
sumE   = (\e ix t ts -> Sum e ix (t:ts))                     -- +{Expr} {Natural} 1>[ {Type}]
      <$> (plus *> expr)
      <*> natural
      <*> typeSig
      <*> some typeSig
prodE  = (\e es -> Product (e:es))                           -- *{Expr}1>[ {Expr}]
      <$> (star *> expr)
      <*> (some expr)
unionE = (\e tyIx t ts -> Union e tyIx $ Set.fromList (t:ts))-- ,{Expr} 1>[ {Type}]
      <$> (comma *> expr)
      <*> typeSig
      <*> typeSig
      <*> (some typeSig)

-- Case expressions
caseE  = Case <$> (textIs "CASE" *> expr) <*> possibleCaseBranches

possibleCaseBranches = caseBranches <|> defaultOnly
defaultOnly          = DefaultOnly  <$> expr
caseBranches         = CaseBranches <$> someCaseBranches <*> ((Just <$> expr) <|> pure Nothing)
someCaseBranches     = SomeCaseBranches <$> (grouped caseBranch) <*> (many (grouped caseBranch))
caseBranch           = CaseBranch <$> caseLHS <*> expr -- {LHS} {Expr}
caseLHS              = alternatives $ map betweenParens [matchTerm,matchSum,matchProduct,matchUnion]

matchArg   = bind
          <|> matchTerm
          <|> (alternatives $ map betweenParens
           [matchTerm
           ,matchSum
           ,matchProduct
           ,matchUnion
           ,bind
           ])
matchTerm    = MatchTerm    <$> termName <*> (many matchArg)
matchSum     = MatchSum     <$> natural <*> matchArg
matchProduct = MatchProduct <$> liftA2 (:) matchArg (some matchArg)
matchUnion   = MatchUnion   <$> typeSig <*> matchArg
bind         = pure Bind    <*  charIs '@'

andExprText :: Text
andExprText = Text.unlines
  ["\\Bool Bool (CASE0"
  ,"               ((#False) #False)"
  ,""
  ,"               (CASE1"
  ,"                   ((#False) #False)"
  ,""
  ,"                   #True"
  ,""
  ,"               )"
  ,"            )"
  ]

