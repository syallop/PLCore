module PL.Expr where

import PL.Type
import PL.Name
import PL.Error
import PL.Var

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad

-- | Small simply typed lambda calculus with term literals and case analysis.
data Expr

    -- | Lambda abstraction
    = Lam
      { take :: Type
      , expr :: Expr
      }

    -- | Application
    | App
      { f :: Expr
      , x :: Expr
      }

    -- | Term literal
    | Term
      { term :: TermName
      }

    -- | Variable
    | Var
      { var :: Var
      }

    -- | Case analysis of an expression
    | Case
      { caseExp      :: Expr
      , caseBranches :: PossibleCaseBranches
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
    = MatchTerm
      {_caseTermName :: TermName   -- ^ The term literal matched upon
      ,_caseMatches  :: [MatchArg] -- ^ Argument pattern
      ,_caseExpr     :: Expr       -- ^ Result of a successful match
      }
    deriving Show

-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg
  = MatchLit TermName [MatchArg] -- ^ Match a term literal (which may be applied to more patterns)
  | BindVar                      -- ^ Match anything and bind it as a variable
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
topExpType :: NameCtx -> Expr -> Either Error Type
topExpType = expType emptyVarCtx

-- | Under a given variable context, type check an expression.
expType :: VarCtx -> NameCtx -> Expr -> Either Error Type
expType varCtx nameCtx e = case e of

  -- | ODDITY/ TODO: Can abstract over types which dont exist..
  --                 They therefore can never be applied.
  --
  --      x : absTy     exp : expTy
  -- ----------------------------------
  --   Lam absTy exp : absTy -> expTy
  Lam absTy exp
    -> do let newVarCtx = addVar absTy varCtx
          expTy <- expType newVarCtx nameCtx exp
          Right $ Arrow absTy expTy

  -- |
  --   f : a -> b    x : a
  -- -----------------------
  --       App f x : b
  App f x
    -> do fTy <- expType varCtx nameCtx f -- Both f and x must type check
          xTy <- expType varCtx nameCtx x

          let errAppMismatch = Left $ EAppMismatch fTy xTy
          case fTy of

            -- Expression of some regular type cannot be applied to things
            Type _
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
  -- caseExp : ct   defExp : t
  -- --------------------------
  --   case caseExp Of
  --      defExp         : t
  Case caseExp (DefaultOnly defExp)
    -> do -- caseExp should be well typed (but we don't care about anything else)
          _ <- expType varCtx nameCtx caseExp

          -- The case expression is then typed by the default branch assuming its well typed
          expType varCtx nameCtx defExp


  -- | A case expression with one or many branches and a possible default branch.
  --
  -- caseExp : ct    defExp : dt    branch0 : t     branches : [t]
  -- -------------------------------------------------------------
  --                   case caseExp of
  --                     branch0
  --                     branches      : t
  --                     mDefExp
  Case caseExp (CaseBranches (SomeCaseBranches branch0 branches) mDefExp)
    -> do -- The expression case-analysed on must type-check
          caseExpTy <- expType varCtx nameCtx caseExp
          -- .. and also must have a Type type
          caseExpTyName <- if isType caseExpTy then Right (_hasType caseExpTy) else Left $ EMsg "Only case analysis on regular (non-function) types is supported"

          -- Check a branches:
          -- - Term exists
          -- - Is applied to the correct number and type of arguments
          -- - RHS expression is well typed under any new variables bound in the match
          --
          -- return the RHS type
          let checkBranch :: CaseBranch -> Either Error Type
              checkBranch branch = do
                  -- The expected param and type of the given term name
                  TermInfo termParamTys termBelongs <- maybe (Left $ EMsg "branch isnt a known term") Right $ Map.lookup (_caseTermName branch) nameCtx

                  -- Must match on the same type as the case expression
                  _ <- if caseExpTyName /= termBelongs then Left $ EMsg "branch matches on a term from a different type" else Right ()

                  -- check the match term is applied to the correct number of args
                  -- - return any bound vars that should be entered into the RHS exps context
                  bindVars <- checkTermMatchArgs (_caseMatches branch) termParamTys nameCtx

                  -- type check the RHS under any newy bound vars
                  expType (addVars bindVars varCtx) nameCtx (_caseExpr branch)


          -- Check the first and any other branches
          branch0Ty <- checkBranch branch0
          branchTys <- mapM checkBranch branches
          -- Check the default branch if it exists
          mDefExpTy <- maybe (Right Nothing) (\defExp -> Just <$> expType varCtx nameCtx defExp) mDefExp

          -- Check all branches have the same result type
          -- If the default branch exists, its type must be the same as the first branch
          _<- maybe (Right ())
                    (\defExpTy -> if defExpTy == branch0Ty then Right () else Left $ EMsg "Default branch and first case branch have different result types")
                    mDefExpTy
          -- Any other branches must have the same type as the first
          _<- mapM (\branchTy -> if branchTy == branch0Ty then Right () else Left $ EMsg "Branch and first branch have different result types")
                   branchTys

          Right branch0Ty
          -- TODO: maybe check coverage...


-- | The matchArgs must correspond to the expected types, same length, same order. Return the list of types that are bound in order by the pattern.
checkTermMatchArgs :: [MatchArg] -> [Type] -> NameCtx -> Either Error [Type]
checkTermMatchArgs matchArgs expectedTs nameCtx = case (matchArgs,expectedTs) of
    ([],[]) -> Right []

    ([],ts) -> Left $ EMsg "Expected more types in match pattern"
    (ts,[]) -> Left $ EMsg "Too many types in match pattern"

    ((BindVar:ms),(t:ts))
      -> do boundTs <- checkTermMatchArgs ms ts nameCtx
            Right $ t : boundTs

    (((MatchLit l nestedMatchArgs):ms),(t:ts))
      -> do -- The expected param and type of the given term name
            TermInfo termParamTys termBelongs <- maybe (Left $ EMsg "branch matches on an unknown term") Right $ Map.lookup l nameCtx

            -- Lit must have the expected type
            _ <- if t /= (Type termBelongs) then Left $ EMsg "branch matches on a term from a different type" else Right ()

            -- And it must be applied to the correct number of args
            -- - return any bound vars that should be entered into the RHS exps context
            boundTs <- checkTermMatchArgs nestedMatchArgs termParamTys nameCtx

            -- Check the rest of the pattern
            boundTs2 <-checkTermMatchArgs ms ts nameCtx
            Right $ boundTs ++ boundTs2

