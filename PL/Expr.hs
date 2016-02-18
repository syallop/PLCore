{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module PL.Expr where

import PL.Type
import PL.TypeCtx
import PL.Name
import PL.Error

import PL.Binds
import PL.Abstracts

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Applicative

todo :: String -> a
todo str = error $ "TODO: " ++ str


type BindAbs b abs = (Binds b, Abstracts abs)
type ExprOf b abs =  BindAbs b abs => Expr b abs

-- | Small simply typed lambda calculus with term literals and case analysis.
-- 'abs' is the type of abstractions
-- 'b' is the type of bindings
data Expr b abs

    -- | Lambda abstraction
    = Lam
      {_take :: abs
      ,_expr :: Expr b abs
      }

    -- | Application
    | App
      {_f :: Expr b abs
      ,_x :: Expr b abs
      }

    -- | Term literal
    | Term
      {_term :: TermName
      }

    -- | Binding
    | Binding
      {_binding :: b
      }

    -- | Case analysis of an expression
    | Case
      {_caseExpr     :: Expr b abs
      ,_caseBranches :: PossibleCaseBranches b abs
      }

    -- | An expression is indexed within a Sum type
    --
    -- Currently assuming type guarantees index is within bounds of sumType
    -- (Not unless type-checked that that the expr has that type)
    | Sum
      {_sumExpr  :: Expr b abs
      ,_sumIndex :: Int
      ,_sumType  :: [Type]
      }

    -- | An expression is a product of many expressions
    | Product
      {_prodExprs :: [Expr b abs]
      }

    -- | An expression has one of many unique types
    | Union
      {_unionExpr      :: Expr b abs
      ,_unionTypeIndex :: Type
      ,_unionType      :: Set.Set Type
      }

deriving instance (Binds b, Abstracts abs) => Eq (Expr b abs)

instance (Binds b,Abstracts abs) => Show (Expr b abs) where
  show = showExpr

-- | Body of a case expression. Is either:
-- - Just a catch all match
-- - One or many branches and a possible default catch all
data PossibleCaseBranches b abs

  -- | No proper matches, only a default catch all
  -- case ... of
  --   {_ -> exp}
  = DefaultOnly
    { _onMatch :: Expr b abs
    }

  -- | One or many branches and a possible default catch all
  -- case ... of
  --   {pat -> exp
  --    ... -> ...
  --    ... -> ...
  --    _   -> ...
  --   }
  | CaseBranches
    { _branches       :: SomeCaseBranches b abs -- One to many
    , _defaultOnMatch :: Maybe (Expr b abs) -- Possible catch all default
    }
    deriving (Show, Eq)

-- | One or many 'CaseBranch'
data SomeCaseBranches b abs = SomeCaseBranches (CaseBranch b abs) [CaseBranch b abs]
  deriving (Show,Eq)

-- | A single branch in a case analysis of a type
data CaseBranch b abs = CaseBranch
  {_caseLHS :: MatchArg
  ,_caseRHS :: Expr b abs
  }
  deriving (Show,Eq)


-- Map a monadic function over all the sub expressions within an expression
{-exprMapM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr-}

-- Map a function over all contained subexpressions.
-- The function should preserve the type of the expression.
mapSubExpressions :: (Expr b abs -> Expr b abs) -> Expr b abs -> Expr b abs
mapSubExpressions f = \case
  Lam ty expr
    -> Lam ty $ f expr

  App fExpr xExpr
    -> App (f fExpr) (f xExpr)

  Case caseExpr possibleBranches
    -> Case (f caseExpr) (case possibleBranches of
                             DefaultOnly branchExpr
                               -> DefaultOnly (f branchExpr)

                             CaseBranches (SomeCaseBranches branch branches) mExpr
                               -> CaseBranches (SomeCaseBranches (mapCaseRHSs f branch) (map (mapCaseRHSs f) branches)) (f <$> mExpr)
                         )

  Sum expr ix ty
    -> Sum (f expr) ix ty

  Product prodExprs
    -> Product (map f prodExprs)

  Union unionExpr tyIx ty
    -> Union (f unionExpr) tyIx ty

mapCaseRHSs :: (Expr b abs -> Expr b abs) -> CaseBranch b abs -> CaseBranch b abs
mapCaseRHSs f (CaseBranch lhs rhs) = CaseBranch lhs (f rhs)



-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg
  = MatchTerm    TermName [MatchArg] -- ^ Match a term literal (which may be applied to more patterns)
  | MatchSum     Int       MatchArg  -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProduct          [MatchArg] -- ^ Match against a product of many types (which may be applied to more patterns)
  | MatchUnion   Type      MatchArg  -- ^ Match against a union of alternatives
  | Bind                             -- ^ Match anything and bind it
  deriving (Show,Eq)

-- | Describes some Term which takes zero or many typed params
-- in order to construct a value of some type it belongs to.
data TermInfo = TermInfo
  { _termParams  :: [Type]   -- ^ A term takes zero or many types
  , _termBelongs :: TypeName -- ^ In order to construct a type called
  }

showExpr :: BindAbs b abs => Expr b abs -> String
showExpr = \case
  Lam takeTy expr
    -> "\\" ++ show (absTy takeTy) ++ "." ++ showExpr expr

  App f x
    -> "( " ++ showExpr f ++ " )" ++ "( " ++ showExpr x ++ " )"

  Term termName
    -> "#" ++ show termName

  Binding b
    -> show b

  Case caseExpr caseBranches
    -> "(CASE " ++ showExpr caseExpr ++ " OF\n" ++ showPossibleCaseBranches caseBranches ++ ")"

  Sum sumExpr sumIndex sumType
    -> let showSumT :: Int -> [Type] -> [String]
           showSumT 0 (t:ts) = ("_" ++ show t ++ "_") : map show ts
           showSumT n (t:ts) = show t : showSumT (n-1) ts
          in showExpr sumExpr ++ " : " ++ (intercalate "|" $ showSumT sumIndex sumType)

  Product prodExprs
    -> "( " ++ (intercalate "," $ map (\e -> "( " ++ showExpr e ++ ") ") prodExprs) ++ ") "

  Union unionExpr unionTypeIndex unionTy
    -> let showUnionT :: Type -> [Type] -> [String]
           showUnionT tyIx (t:ts)
            | tyIx == t = ("_" ++ show t ++ "_") : map show ts
            | otherwise = show t : showUnionT tyIx ts
          in showExpr unionExpr ++ " : <" ++ (intercalate "|" $ showUnionT unionTypeIndex (Set.toList unionTy)) ++ ">"

showPossibleCaseBranches :: BindAbs b abs => PossibleCaseBranches b abs -> String
showPossibleCaseBranches = \case
  DefaultOnly onMatch
    -> "DEFAULT -> " ++ showExpr onMatch

  CaseBranches someCaseBranches maybeDefaultOnMatch
    -> showSomeCaseBranches someCaseBranches ++ "\n\n" ++ (maybe "" (("DEFAULT -> " ++) . showExpr) maybeDefaultOnMatch)

showSomeCaseBranches :: BindAbs b abs => SomeCaseBranches b abs -> String
showSomeCaseBranches (SomeCaseBranches caseBranch caseBranches) = intercalate "\n\n" $ map showCaseBranch (caseBranch : caseBranches)

showCaseBranch :: BindAbs b abs => CaseBranch b abs -> String
showCaseBranch (CaseBranch lhs rhs) = showMatchArg lhs ++ " -> " ++ showExpr rhs

showMatchArgs :: [MatchArg] -> String
showMatchArgs = intercalate " " . map showMatchArg

showMatchArg :: MatchArg -> String
showMatchArg = \case
  MatchTerm name matches
    -> "#" ++ show name ++ " " ++ showMatchArgs matches

  MatchSum ix matchArg
    -> show ix ++ "| " ++ showMatchArg matchArg

  MatchProduct matchArgs
    -> intercalate "," $ map showMatchArg matchArgs

  MatchUnion ty matchArg
    -> show ty ++ "| " ++ showMatchArg matchArg

  Bind
    -> "b"

-- | Map term names to their params and the type they belong to.
type NameCtx = Map.Map TermName TermInfo

-- | A top-level expression is an expression without a bindings context.
topExprType :: BindAbs b abs => NameCtx -> Expr b abs -> Either Error Type
topExprType = exprType emptyCtx

-- | Under a given bindings context, type check an expression.
exprType :: forall b abs. BindAbs b abs => BindCtx b -> NameCtx -> Expr b abs -> Either Error Type
exprType bindCtx nameCtx e = case e of


  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam abs expr
    -> do let newBindCtx = addBinding (absTy abs) bindCtx
          exprTy <- exprType newBindCtx nameCtx expr
          Right $ Arrow (absTy abs) exprTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType bindCtx nameCtx f -- Both f and x must type check
          xTy <- exprType bindCtx nameCtx x

          let errAppMismatch = Left $ EAppMismatch fTy xTy
          case fTy of
            -- Regular function application attempt
            Arrow aTy bTy
              | aTy == xTy -> Right bTy
              | otherwise  -> errAppMismatch

            _ -> errAppMismatch


  -- | A term is looked up in the nameCtx and is typed to accept all of its parameters and produce the type is belongs to.
  -- It is assumed the nameCtx has been type checked => each of these parameters have been type checked
  --
  -- termName:t IN nameCtx
  -- -----------------------
  --      termName : t
  Term termName
    -> do TermInfo params belongs <- maybe (Left $ ETermNotDefined termName) Right $ Map.lookup termName nameCtx
          Right $ arrowise (params ++ [ty belongs])

  -- Provided an expression type checks and its type is in the correct place within a sum,
  -- has that sum type.
  Sum expr ix inTypr
    -> do -- Expression must type check
          exprTy <- exprType bindCtx nameCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          _ <- if exprTy /= (inTypr !! ix) then Left $ EMsg "Expression doesnt have the type of the position in a sum type it claims it has" else Right ()

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Product prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType bindCtx nameCtx) prodExprs

          -- the type is the product of those types
          Right $ ProductT prodExprTys

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  Union unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType bindCtx nameCtx unionExpr

          -- Type must be what we claim it is...
          _ <- if exprTy /= unionTypeIndex then Left $ EMsg "Expresiion doesnt have the type within the union it claims to have" else Right ()

          -- Type must be in the set somewhere...
          _ <- if exprTy `Set.member` unionTypes then Right () else Left $ EMsg "Expressions type is not within the union"

          -- the type is the claimed union
          Right $ UnionT unionTypes

  -- | A binding is typed by the context
  -- It is assumed the bindCtx has been type checked
  --
  -- b : t IN bindCtx
  -- -----------------
  --      b : t
  Binding b
    -> Right $ b `bindTy` bindCtx

  -- | A case expression with only a defaut branch.
  --
  -- caseExpr : ct   defExpr : t
  -- --------------------------
  --   case caseExpr Of
  --      defExpr        : t
  Case caseExpr (DefaultOnly defExpr)
    -> do -- caseExpr should be well typed (but we don't care about anything else)
          _ <- exprType bindCtx nameCtx caseExpr

          -- The case expression is then typed by the default branch assuming its well typed
          exprType bindCtx nameCtx defExpr


  -- | A case expression with one or many branches and a possible default branch.
  --
  -- caseExpr : ct    defExpr : dt    branch0 : t     branches : [t]
  -- -------------------------------------------------------------
  --                   case caseExpr of
  --                     branch0
  --                     branches      : t
  --                     mDefExpr
  Case caseExpr (CaseBranches (SomeCaseBranches branch0 branches) mDefExpr)
    -> do -- The expression case-analysed on must type-check
          caseExprTy <- exprType bindCtx nameCtx caseExpr

          -- Check the first and any other branches
          branch0Ty <- branchType branch0 caseExprTy bindCtx nameCtx
          branchTys <- mapM (\branch -> branchType branch caseExprTy bindCtx nameCtx) branches

          -- Check the default branch if it exists
          mDefExprTy <- maybe (Right Nothing) (\defExpr -> Just <$> exprType bindCtx nameCtx defExpr) mDefExpr

          -- Check all branches have the same result type
          -- If the default branch exists, its type must be the same as the first branch
          _<- maybe (Right ())
                    (\defExprTy -> if defExprTy == branch0Ty then Right () else Left $ EMsg "Default branch and first case branch have different result types")
                    mDefExprTy
          -- Any other branches must have the same type as the first
          _<- mapM (\branchTy -> if branchTy == branch0Ty then Right () else Left $ EMsg "Branch and first branch have different result types")
                   branchTys

          Right branch0Ty
          -- TODO: maybe check coverage...

-- Type check a case branch, requring it match the expected type under a namectx
-- if so, type checking the result expression which is returned
branchType :: BindAbs b abs => CaseBranch b abs -> Type -> BindCtx b -> NameCtx -> Either Error Type
branchType (CaseBranch lhs rhs) expectedTy bindCtx nameCtx = do
  bindings <- checkMatchWith lhs expectedTy nameCtx
  exprType (addBindings bindings bindCtx) nameCtx rhs

-- | Check that a MatchArg matches the expected Type under a NameCtx.
-- If so, return a list of types of any bound bindings.
checkMatchWith :: MatchArg -> Type -> NameCtx -> Either Error [Type]
checkMatchWith match expectTy nameCtx = case match of

    -- Bind the value
    Bind
      -> Right [expectTy]

    MatchTerm termLit nestedMatchArgs
      -> do -- The expected param and type of the given term name
            TermInfo termParamTys termBelongs <- maybe (Left $ EMsg "pattern matches on unknown term literal") Right $ Map.lookup termLit nameCtx

            -- Lit Must have the expected type Type
            _ <- if Named termBelongs /= expectTy then Left $ EMsg "pattern matches on a term from a different type" else Right ()

            -- Lit must be applied to the correct number of pattern args which must themselves be correctly typed and may bind nested bindings
            checkMatchesWith nestedMatchArgs termParamTys nameCtx

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case expectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left $ EMsg "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if length sumTypes < sumIndex then Left $ EMsg "Matching on a larger sum index than the sum type contains" else Right (sumTypes !! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy nameCtx

    MatchProduct nestedMatchArgs
      -> do prodTypes <- case expectTy of
                             ProductT prodTypes -> Right prodTypes
                             _               -> Left $ EMsg "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes nameCtx

    MatchUnion unionIndexTy nestedMatchArg
      -> do unionTypes <- case expectTy of
                        UnionT unionTypes -> Right unionTypes
                        _                 -> Left $ EMsg "Expected union type in pattern match"

            -- type index must be a member of the union alternatives
            _ <- if Set.member unionIndexTy unionTypes then Right () else Left $ EMsg "Matching on a type which isnt a member of the union"

            -- must actually match on the expected type
            checkMatchWith nestedMatchArg unionIndexTy nameCtx


checkMatchesWith :: [MatchArg] -> [Type] -> NameCtx -> Either Error [Type]
checkMatchesWith matches types nameCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],_)  -> Left $ EMsg "Expected more patterns in match"
  (_,[])  -> Left $ EMsg "Too many patterns in match"
  (m:ms,t:ts)
    -> checkMatchWith m t nameCtx >>= \boundTs -> checkMatchesWith ms ts nameCtx >>= Right . (boundTs ++)

-- Bind a list of expression with claimed types within an expression
-- by transforming to an application to lambda abstractions.
-- I.E. each subsequent expression
{-lets :: [(Expr,Type)] -> Expr -> Expr-}
{-lets bind exp = foldr (flip App) (foldr Lam exp $ reverse types) exprs-}
  {-where-}
    {-exprs = map fst bind-}
    {-types = map snd bind-}

appise :: [Expr b abs] -> Expr b abs
appise (e:[])    = e
appise (e:e':es) = appise ((App e e'):es)

lamise :: BindAbs b abs => [abs] -> Expr b abs -> Expr b abs
lamise (t:[])    e = Lam t e
lamise (t:t':ts) e = Lam t (lamise (t':ts) e)

