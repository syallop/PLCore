{-# LANGUAGE LambdaCase #-}
module PL.Expr where

import PL.Type
import PL.Name
import PL.Error
import PL.Var

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Applicative

todo :: String -> a
todo str = error $ "TODO: " ++ str

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

    -- | An expression has one of many unique types
    | Union
      {_unionExpr      :: Expr
      ,_unionTypeIndex :: Type
      ,_unionType      :: Set.Set Type
      }
    deriving (Eq)

instance Show Expr where
  show = showExpr

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
    deriving (Show, Eq)

-- | One or many 'CaseBranch'
data SomeCaseBranches = SomeCaseBranches CaseBranch [CaseBranch]
  deriving (Show,Eq)

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

    -- | Match against a union of types
    | CaseUnion
      {_caseUnionTypeIndex :: Type
      ,_caseUnionMatch     :: MatchArg
      ,_caseUnionExpr      :: Expr
      }
    deriving (Show,Eq)

unCaseBranch :: CaseBranch -> (MatchArg,Expr)
unCaseBranch = \case
  CaseTerm  name    matches rhs -> (MatchTerm  name    matches, rhs)
  CaseSum   ix      match   rhs -> (MatchSum   ix      match  , rhs)
  CaseProd  matches         rhs -> (MatchProd  matches        , rhs)
  CaseUnion tyIx    match   rhs -> (MatchUnion tyIx    match  , rhs)

rhs :: CaseBranch -> Expr
rhs = \case
  CaseTerm  _ _ rhs -> rhs
  CaseSum   _ _ rhs -> rhs
  CaseProd  _   rhs -> rhs
  CaseUnion _ _ rhs -> rhs



-- Map a monadic function over all the sub expressions within an expression
{-exprMapM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr-}

-- Map a function over all contained subexpressions.
-- The function should preserve the type of the expression.
mapSubExpressions :: (Expr -> Expr) -> Expr -> Expr
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

  Prod prodExprs
    -> Prod (map f prodExprs)

  Union unionExpr tyIx ty
    -> Union (f unionExpr) tyIx ty

mapCaseRHSs :: (Expr -> Expr) -> CaseBranch -> CaseBranch
mapCaseRHSs f = \case
    CaseTerm  name    matches expr -> CaseTerm  name    matches (f expr)
    CaseSum   ix      match   expr -> CaseSum   ix      match   (f expr)
    CaseProd  matches         expr -> CaseProd  matches         (f expr)
    CaseUnion tyIx    match   expr -> CaseUnion tyIx    match   (f expr)



-- | Argument pattern in a case statements match.
-- case ... of
--  T {A b (C d E)} -> ...
data MatchArg
  = MatchTerm TermName [MatchArg] -- ^ Match a term literal (which may be applied to more patterns)
  | MatchSum  Int      MatchArg   -- ^ Match against a sum alternative (which may be applied to more patterns)
  | MatchProd [MatchArg]          -- ^ Match against a product of many types (which may be applied to more patterns)
  | MatchUnion Type MatchArg      -- ^ Match against a union of alternatives
  | BindVar                       -- ^ Match anything and bind it as a variable
  deriving (Show,Eq)

-- | Describes some Term which takes zero or many typed params
-- in order to construct a value of some type it belongs to.
data TermInfo = TermInfo
  { _termParams  :: [Type]   -- ^ A term takes zero or many types
  , _termBelongs :: TypeName -- ^ In order to construct a type called
  }

showExpr :: Expr -> String
showExpr = \case
  Lam takeTy expr
    -> "\\" ++ show takeTy ++ "." ++ showExpr expr

  App f x
    -> "( " ++ showExpr f ++ " )" ++ "( " ++ showExpr x ++ " )"

  Term termName
    -> "#" ++ show termName

  Var v
    -> show $ varToInt v

  Case caseExpr caseBranches
    -> "(CASE " ++ showExpr caseExpr ++ " OF\n" ++ showPossibleCaseBranches caseBranches ++ ")"

  Sum sumExpr sumIndex sumType
    -> let showSumT :: Int -> [Type] -> [String]
           showSumT 0 (t:ts) = ("_" ++ show t ++ "_") : map show ts
           showSumT n (t:ts) = show t : showSumT (n-1) ts
          in showExpr sumExpr ++ " : " ++ (intercalate "|" $ showSumT sumIndex sumType)

  Prod prodExprs
    -> "( " ++ (intercalate "," $ map (\e -> "( " ++ showExpr e ++ ") ") prodExprs) ++ ") "

  Union unionExpr unionTypeIndex unionTy
    -> let showUnionT :: Type -> [Type] -> [String]
           showUnionT tyIx (t:ts)
            | tyIx == t = ("_" ++ show t ++ "_") : map show ts
            | otherwise = show t : showUnionT tyIx ts
          in showExpr unionExpr ++ " : <" ++ (intercalate "|" $ showUnionT unionTypeIndex (Set.toList unionTy)) ++ ">"

showPossibleCaseBranches :: PossibleCaseBranches -> String
showPossibleCaseBranches = \case
  DefaultOnly onMatch
    -> "DEFAULT -> " ++ showExpr onMatch

  CaseBranches someCaseBranches maybeDefaultOnMatch
    -> showSomeCaseBranches someCaseBranches ++ "\n\n" ++ (maybe "" (("DEFAULT -> " ++) . showExpr) maybeDefaultOnMatch)

showSomeCaseBranches :: SomeCaseBranches -> String
showSomeCaseBranches (SomeCaseBranches caseBranch caseBranches) = intercalate "\n\n" $ map showCaseBranch (caseBranch : caseBranches)

showCaseBranch :: CaseBranch -> String
showCaseBranch = \case
    CaseTerm name matches expr
      -> "#" ++ show name ++ " " ++ showMatchArgs matches
             ++ " -> " ++ showExpr expr

    CaseSum ix match expr
      -> showMatchArg (MatchSum ix match) ++ " -> " ++ showExpr expr

    CaseProd matches expr
      -> showMatchArg (MatchProd matches) ++ " -> " ++ showExpr expr

    CaseUnion ty match expr
      -> showMatchArg (MatchUnion ty match) ++ " -> " ++ showExpr expr

showMatchArgs :: [MatchArg] -> String
showMatchArgs = intercalate " " . map showMatchArg

showMatchArg :: MatchArg -> String
showMatchArg = \case
  MatchTerm name matches
    -> "#" ++ show name ++ " " ++ showMatchArgs matches

  MatchSum ix matchArg
    -> show ix ++ "| " ++ showMatchArg matchArg

  MatchProd matchArgs
    -> intercalate "," $ map showMatchArg matchArgs

  MatchUnion ty matchArg
    -> show ty ++ "| " ++ showMatchArg matchArg

  BindVar
    -> "b"

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
          exprTy <- exprType varCtx nameCtx expr

          -- Expression must have the type of the index in the sum it claims to have...
          _ <- if exprTy /= (inTypr !! ix) then Left $ EMsg "Expression doesnt have the type of the position in a sum type it claims it has" else Right ()

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

  -- Provided an expression typechecks and its type exists within the union, it has the claimed union type.
  Union unionExpr unionTypeIndex unionTypes
    -> do -- type check injected expression
          exprTy <- exprType varCtx nameCtx unionExpr

          -- Type must be what we claim it is...
          _ <- if exprTy /= unionTypeIndex then Left $ EMsg "Expresiion doesnt have the type within the union it claims to have" else Right ()

          -- Type must be in the set somewhere...
          _ <- if exprTy `Set.member` unionTypes then Right () else Left $ EMsg "Expressions type is not within the union"

          -- the type is the claimed union
          Right $ UnionT unionTypes

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

    CaseUnion caseUnionTypeIndex caseUnionMatch caseUnionExpr
      -> do -- must be well-typed and have the same type as the case expression
            bindVars <- checkMatchWith (MatchUnion caseUnionTypeIndex caseUnionMatch) caseExprTy nameCtx

            -- Type check the RHS under any newly bound vars
            exprType (addVars bindVars varCtx) nameCtx caseUnionExpr

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
            _ <- if Type termBelongs /= expectTy then Left $ EMsg "pattern matches on a term from a different type" else Right ()

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

appise :: [Expr] -> Expr
appise (e:[])    = e
appise (e:e':es) = App (App e e') (appise es)
