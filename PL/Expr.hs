module PL.Expr where

import PL.Type
import PL.Name
import PL.Error
import PL.Var

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

todo = error "TODO"

-- | Small simply typed lambda calculus with term literals and case analysis.
data Expr

    -- | Lambda abstraction
    = Lam
      {_take :: Type
      ,_expr :: Expr
      }

    -- | Application
    | App
      {_f :: Expr
      ,_x :: Expr
      }

    -- | Term literal
    | Term
      {_term :: TermName
      }

    -- | Variable
    | Var
      {_var :: Var
      }

    -- | Case analysis of an expression
    | Case
      {_caseExpr     :: Expr
      ,_caseBranches :: PossibleCaseBranches
      }

    -- | An expression is indexed within a Sum type
    --
    -- Currently assuming type guarantees index is within bounds of sumType
    -- (Not unless type-checked that that the expr has that type)
    | Sum
      {_sumExpr  :: Expr
      ,_sumIndex :: Int
      ,_sumType  :: [Type]
      }

    -- | An expression is a product of many expressions
    | Prod
      {_prodExprs :: [Expr]
      }
    deriving Show

-- | Body of a case expression. Is either:
-- - Just a catch all match
-- - One or many branches and a possible default catch all
data PossibleCaseBranches

  -- | No proper matches, only a default catch all
  -- case ... of
  --   {_ -> exp}
  = DefaultOnly
    { _onMatch :: Expr
    }

  -- | One or many branches and a possible default catch all
  -- case ... of
  --   {pat -> exp
  --    ... -> ...
  --    ... -> ...
  --    _   -> ...
  --   }
  | CaseBranches
    { _branches       :: SomeCaseBranches -- One to many
    , _defaultOnMatch :: Maybe Expr       -- Possible catch all default
    }
    deriving Show

-- | One or many 'CaseBranch'
data SomeCaseBranches = SomeCaseBranches CaseBranch [CaseBranch]
  deriving Show

-- | A single branch in a case analysis of a type.
-- case ... of
--   {T A b (C d E) -> exp}
data CaseBranch

    -- | Match against a fully applied term
    -- (where parameters are implicitly bound with de brujn indices)
    = CaseTerm
      {_caseTermName    :: TermName   -- ^ The term literal matched upon
      ,_caseTermMatches :: [MatchArg] -- ^ Argument pattern
      ,_caseTermExpr    :: Expr       -- ^ Result of a successful match
      }

    -- | Match against an indexed alternative in a sum
    | CaseSum
      {_caseSumIndex :: Int
      ,_caseSumMatch :: MatchArg
      ,_caseSumExpr  :: Expr
      }

    -- | Match against a product of types
    | CaseProd
      {_caseProdMatches :: [MatchArg]
      ,_caseProdExpr    :: Expr
      }
    deriving Show

-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg
  = MatchTerm TermName [MatchArg] -- ^ Match a term literal (which may be applied to more patterns)
  | MatchSum  Int      MatchArg   -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProd [MatchArg]          -- ^ Match against a product of many types (which may be applied to more patterns)
  | BindVar                       -- ^ Match anything and bind it as a variable
  deriving Show

-- | Describes some Term which takes zero or many typed params
-- in order to construct a value of some type it belongs to.
data TermInfo = TermInfo
  { _termParams  :: [Type]   -- ^ A term takes zero or many types
  , _termBelongs :: TypeName -- ^ In order to construct a type called
  }

-- | Map term names to their params and the type they belong to.
type NameCtx = Map.Map TermName TermInfo

-- | A top-level expression is an expression without a variable context.
topExprType :: NameCtx -> Expr -> Either Error Type
topExprType = exprType emptyVarCtx

-- | Under a given variable context, type check an expression.
exprType :: VarCtx -> NameCtx -> Expr -> Either Error Type
exprType varCtx nameCtx e = case e of

  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     expr : exprTy
  -- ----------------------------------
  --   Lam absTy expr : absTy -> exprTy
  Lam absTy expr
    -> do let newVarCtx = addVar absTy varCtx
          exprTy <- exprType newVarCtx nameCtx expr
          Right $ Arrow absTy exprTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- exprType varCtx nameCtx f -- Both f and x must type check
          xTy <- exprType varCtx nameCtx x

          let errAppMismatch = Left $ EAppMismatch fTy xTy
          case fTy of

            -- Expression of some regular type cannot be applied to things
            Type _
              -> errAppMismatch

            -- Expression of a sum type cannot be applied to things
            SumT _
              -> errAppMismatch

            -- 1. Regular function application attempt
            Arrow aTy bTy
              | aTy == xTy -> Right bTy
              | otherwise  -> errAppMismatch


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
  Sum expr index inTypr
    -> do -- Expression must type check
          exprTy <- exprType varCtx nameCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          _ <- if exprTy /= (inTypr !! index) then Left $ EMsg "Expression doesnt have the type of the position in a sum type it claims it has" else Right ()

          -- Allow the other types in the sum to not exist...
          _ <- Right ()

          -- Type is the claimed sum
          Right $ SumT inTypr

  -- A product is typed by the order of each expression it contains
  Prod prodExprs
    -> do -- type check each successive expression
          prodExprTys <- mapM (exprType varCtx nameCtx) prodExprs

          -- the type is the product of those types
          Right $ ProdT prodExprTys


  -- | A variable is typed by the context
  -- It is assumed the varCtx has been type checked
  --
  -- var : t IN varCtx
  -- -----------------
  --      var : t
  Var var
    -> Right $ var `index` varCtx

  -- | A case expression with only a defaut branch.
  --
  -- caseExpr : ct   defExpr : t
  -- --------------------------
  --   case caseExpr Of
  --      defExpr        : t
  Case caseExpr (DefaultOnly defExpr)
    -> do -- caseExpr should be well typed (but we don't care about anything else)
          _ <- exprType varCtx nameCtx caseExpr

          -- The case expression is then typed by the default branch assuming its well typed
          exprType varCtx nameCtx defExpr


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
          caseExprTy <- exprType varCtx nameCtx caseExpr

          -- Check the first and any other branches
          branch0Ty <- branchType branch0 caseExprTy varCtx nameCtx
          branchTys <- mapM (\branch -> branchType branch caseExprTy varCtx nameCtx) branches

          -- Check the default branch if it exists
          mDefExprTy <- maybe (Right Nothing) (\defExpr -> Just <$> exprType varCtx nameCtx defExpr) mDefExpr

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
branchType :: CaseBranch -> Type -> VarCtx -> NameCtx -> Either Error Type
branchType caseBranch caseExprTy varCtx nameCtx = case caseBranch of
    CaseTerm caseTermName caseTermMatches caseTermExpr
       -> do -- must be well-typed and have the same type as the case expression
             bindVars <- checkMatchWith (MatchTerm caseTermName caseTermMatches) caseExprTy nameCtx

             -- Type check the RHS under any newly bound vars
             exprType (addVars bindVars varCtx) nameCtx caseTermExpr

    CaseSum caseSumIndex caseSumMatch caseSumExpr
      -> do -- must be well-typed and have the same type as the case expression
            bindVars <- checkMatchWith (MatchSum caseSumIndex caseSumMatch) caseExprTy nameCtx

            -- Type check the RHS under any newly bound vars
            exprType (addVars bindVars varCtx) nameCtx caseSumExpr

    CaseProd caseProdMatches caseProdExpr
      -> do -- must be well-typed and have the same type as the case expression
            bindVars <- checkMatchWith (MatchProd caseProdMatches) caseExprTy nameCtx

            -- Type check the RHS under any newly bound vars
            exprType (addVars bindVars varCtx) nameCtx caseProdExpr

-- | Check that a MatchArg matches the expected Type under a NameCtx.
-- If so, return a list of types of any bound variables.
checkMatchWith :: MatchArg -> Type -> NameCtx -> Either Error [Type]
checkMatchWith match expectTy nameCtx = case match of

    -- Bind the value to a variable
    BindVar
      -> Right [expectTy]

    MatchTerm termLit nestedMatchArgs
      -> do -- The expected param and type of the given term name
            TermInfo termParamTys termBelongs <- maybe (Left $ EMsg "pattern matches on unknown term literal") Right $ Map.lookup termLit nameCtx

            -- Lit Must have the expected type Type
            _ <- if (Type termBelongs) /= expectTy then Left $ EMsg "pattern matches on a term from a different type" else Right ()

            -- Lit must be applied to the correct number of pattern args which must themselves be correctly typed and may bind nested vars
            checkMatchesWith nestedMatchArgs termParamTys nameCtx

    MatchSum sumIndex nestedMatchArg
      -> do sumTypes <- case expectTy of
                      SumT sumTypes -> Right sumTypes
                      _             -> Left $ EMsg "Expected sum type in pattern match"

            -- index must be within the number of alternative in the sum type
            matchedTy <- if length sumTypes < sumIndex then Left $ EMsg "Matching on a larger sum index than the sum type contains" else Right (sumTypes !! sumIndex)

            -- must have the expected index type
            checkMatchWith nestedMatchArg matchedTy nameCtx

    MatchProd nestedMatchArgs
      -> do prodTypes <- case expectTy of
                             ProdT prodTypes -> Right prodTypes
                             _               -> Left $ EMsg "Expected product type in pattern match"

            checkMatchesWith nestedMatchArgs prodTypes nameCtx

checkMatchesWith :: [MatchArg] -> [Type] -> NameCtx -> Either Error [Type]
checkMatchesWith matches types nameCtx = case (matches,types) of
  ([],[]) -> Right []
  ([],ts) -> Left $ EMsg "Expected more patterns in match"
  (ms,[]) -> Left $ EMsg "Too many patterns in match"
  ((m:ms),(t:ts))
    -> checkMatchWith m t nameCtx >>= \boundTs -> checkMatchesWith ms ts nameCtx >>= Right . (boundTs ++)

