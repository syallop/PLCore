{-|
Module      : PL.Case
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Case analysis on... things.
-}
module PL.Case where

import Prelude hiding (sequence,mapM,foldr)

import PL.ExprLike
import PLPrinter hiding (parens,between)
import PLPrinter.Doc

import Control.Applicative
import Data.Foldable
import Data.List.NonEmpty
import Data.Monoid
import Data.Traversable (sequence,mapM)
import qualified Data.List.NonEmpty as NonEmpty

-- | Case analysis on a scrutinee 'e' which either:
-- - Is just a default match 'e'
-- or is:
-- - A non-empty list of patterns 'p', 'e' result pairs
--   and a possible default if none of the 'p'atterns match.
data Case e p = Case
  {_caseScrutinee    :: e
  ,_caseCaseBranches :: CaseBranches e p
  }
  deriving (Eq,Show)

-- | Body of a case expression.
-- Like a list of patterns 'p', result expression 'e' pairs with an optional default catch all 'e'.
--
-- This isnt simply a [CaseBranch] and a (Maybe e) because that would allow [] Nothing and we
-- dont (currently) allow empty case expression bodies.
data CaseBranches e p

  -- | No proper patterns, only a default catch all.
  = DefaultOnly
    { _caseBranchesOnMatch :: e
    }

  -- | One or many branches and an optional default catch all.
  | CaseBranches
    { _caseBranches                :: NonEmpty (CaseBranch e p)
    , _caseBranchesOptionalDefault :: Maybe e
    }
  deriving (Eq,Show)

-- | A single branch in a case analysis
data CaseBranch e p = CaseBranch
 {_caseBranchPattern :: p -- The pattern to match
 ,_caseBranchResult  :: e -- The result if a match is successful
 }
 deriving (Eq,Show)

mapCaseExpr :: (e -> a) -> Case e p -> Case a p
mapCaseExpr f c = case c of
  Case e bs -> Case (f e) (mapCaseBranchesExpr f bs)

mapCaseBranchesExpr :: (e -> a) -> CaseBranches e p -> CaseBranches a p
mapCaseBranchesExpr f c = case c of
  DefaultOnly e
    -> DefaultOnly $ f e

  CaseBranches bs me
    -> CaseBranches (mapCaseBranchExpr f <$> bs) (f <$> me)

mapCaseBranchExpr :: (e -> a) -> CaseBranch e p -> CaseBranch a p
mapCaseBranchExpr f c = case c of
  CaseBranch m e -> CaseBranch m (f e)


sequenceCaseExpr :: Monad f => Case (f e) p -> f (Case e p)
sequenceCaseExpr c = case c of
  Case fe fbs -> do e  <- fe
                    bs <- sequenceCaseBranchesExpr fbs
                    return $ Case e bs

sequenceCaseBranchesExpr :: Monad f => CaseBranches (f e) p -> f (CaseBranches e p)
sequenceCaseBranchesExpr c = case c of
  DefaultOnly fe
    -> do e <- fe
          return $ DefaultOnly e

  CaseBranches fbs fmDef
    -> do bs   <- mapM sequenceCaseBranchExpr fbs
          mDef <- sequence fmDef
          return $ CaseBranches bs mDef

sequenceCaseBranchExpr :: Monad f => CaseBranch (f e) p -> f (CaseBranch e p)
sequenceCaseBranchExpr c = case c of
  CaseBranch m fe
    -> do e <- fe
          return $ CaseBranch m e

instance (Document e
         ,Document m
         )
      => Document (Case e m) where
  document (Case s bs) = document s <> document bs

instance (Document e
         ,Document m
         )
      => Document (CaseBranches e m) where
  document bs = case bs of
    DefaultOnly def
      -> parens (document def)

    CaseBranches bs mDef
      -> foldr (\b acc -> acc <> document b) emptyDoc bs <> maybe emptyDoc (parens . document) mDef

instance (Document e
         ,Document m
         )
      => Document (CaseBranch e m) where
  document (CaseBranch m r) = char '|' <> document m <> document r

