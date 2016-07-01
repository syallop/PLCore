module PL.Case where

import Prelude hiding (sequence,mapM)

import PL.ExprLike

import Data.List.NonEmpty
import Control.Applicative
import Data.Traversable (sequence,mapM)

-- | Case analysis on a scrutinee 'e' which either:
-- - Is just a default match 'e'
-- or is:
-- - A non-empty list of 'm' match, 'e' result pairs
--   and a possible default if none of the 'm's match.
data Case e m = Case
  {_caseScrutinee    :: e
  ,_caseCaseBranches :: CaseBranches e m
  }
  deriving (Eq,Show)

-- | Body of a case expression.
-- Like a list of match 'm', result expression 'e' pairs with an optional default catch all 'e'.
--
-- This isnt simply a [CaseBranch] and a (Maybe e) because that would allow [] Nothing and we
-- dont (currently) allow empty case expression bodies.
data CaseBranches e m

  -- | No proper matches, only a default catch all.
  = DefaultOnly
    { _caseBranchesOnMatch :: e
    }

  -- | One or many branches and an optional default catch all.
  | CaseBranches
    { _caseBranches                :: NonEmpty (CaseBranch e m)
    , _caseBranchesOptionalDefault :: Maybe e
    }
  deriving (Eq,Show)

-- | A single branch in a case analysis
data CaseBranch e m = CaseBranch
 {_caseBranchMatch  :: m -- The match-er
 ,_caseBranchResult :: e -- The result if a match is successful
 }
 deriving (Eq,Show)

mapCaseExpr :: (e -> a) -> Case e m -> Case a m
mapCaseExpr f c = case c of
  Case e bs -> Case (f e) (mapCaseBranchesExpr f bs)

mapCaseBranchesExpr :: (e -> a) -> CaseBranches e m -> CaseBranches a m
mapCaseBranchesExpr f c = case c of
  DefaultOnly e
    -> DefaultOnly $ f e

  CaseBranches bs me
    -> CaseBranches (mapCaseBranchExpr f <$> bs) (f <$> me)

mapCaseBranchExpr :: (e -> a) -> CaseBranch e m -> CaseBranch a m
mapCaseBranchExpr f c = case c of
  CaseBranch m e -> CaseBranch m (f e)


sequenceCaseExpr :: Monad f => Case (f e) m -> f (Case e m)
sequenceCaseExpr c = case c of
  Case fe fbs -> do e  <- fe
                    bs <- sequenceCaseBranchesExpr fbs
                    return $ Case e bs

sequenceCaseBranchesExpr :: Monad f => CaseBranches (f e) m -> f (CaseBranches e m)
sequenceCaseBranchesExpr c = case c of
  DefaultOnly fe
    -> do e <- fe
          return $ DefaultOnly e

  CaseBranches fbs fmDef
    -> do bs   <- mapM sequenceCaseBranchExpr fbs
          mDef <- sequence fmDef
          return $ CaseBranches bs mDef

sequenceCaseBranchExpr :: Monad f => CaseBranch (f e) m -> f (CaseBranch e m)
sequenceCaseBranchExpr c = case c of
  CaseBranch m fe
    -> do e <- fe
          return $ CaseBranch m e

