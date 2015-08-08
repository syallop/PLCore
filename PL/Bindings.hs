{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PL.Bindings
  ( BuryDepth()
  , Bindings()
  , Binding(Unbound,Bound)
  , emptyBindings
  , bury
  , bind
  , unbound
  , safeIndex
  , index

  , bindingsFromList
  , append
  ) where


import Control.Applicative

import PL.Expr
import PL.Binds
{-import PL.Var  hiding (index)-}

-- | A positive integer indicating how many lambda abstractions a bound expression has been moved under.
newtype BuryDepth = BuryDepth {_unBurryDepth :: Int} deriving (Num,Eq)

data Bindings b
  = EmptyBindings                        -- ^ No bindings
  | ConsBinding (Binding b) (Bindings b) -- ^ A new binding
  | Buried (Bindings b)                  -- ^ Bury many bindings beneath a lambda abstraction
  deriving Show

data Binding b
  = Unbound        -- No expression bound yet
  | Bound (Expr b) -- This expression is bound
  deriving Show

emptyBindings :: Bindings b
emptyBindings = EmptyBindings

-- | Bury bindings under a lambda abstraction
bury :: Bindings b -> Bindings b
bury = Buried

-- | Introduce an unbound variable
unbound :: Bindings b -> Bindings b
unbound = ConsBinding Unbound

-- | Introduce a new bound variable
bind :: Expr b -> Bindings b -> Bindings b
bind = ConsBinding . Bound

-- append a list of bound expressions to a bindings
append :: [Expr b] -> Bindings b -> Bindings b
append []     bs = bs
append (x:xs) bs = ConsBinding (Bound x) (append xs bs)

-- | Transform a list of expressions to bind into a bindings
bindingsFromList :: [Expr b] -> Bindings b
bindingsFromList []     = EmptyBindings
bindingsFromList (b:bs) = ConsBinding (Bound b) (bindingsFromList bs)

-- | If the index exists, extract the bound expression, itself adjusted for it's depth in the bindings.
safeIndex :: Binds b => Bindings b -> Int -> Maybe (Binding b)
safeIndex EmptyBindings _ = Nothing
safeIndex bs           ix = safeIndex' 0 bs ix
  where
    safeIndex' :: Binds b => BuryDepth -> Bindings b -> Int -> Maybe (Binding b)
    safeIndex' buryDepth bs ix = case (bs,ix) of
      (EmptyBindings    , _) -> Nothing

      (ConsBinding b _  , 0) -> case b of
                                   Unbound    -> Just Unbound
                                   Bound expr -> Just $ Bound $ expr `buryBy` buryDepth
      (ConsBinding _ bs', _) -> safeIndex' buryDepth     bs' (ix-1)

      (Buried        bs', _) -> safeIndex' (buryDepth+1) bs' ix

-- | 'safeIndex' assuming the index is contained in the bindings.
index :: Binds b => Bindings b -> Int -> Binding b
index bs ix = let Just r = safeIndex bs ix in r

-- bury any escaping variables in an expression by given depth.
--
-- E.G.
-- Unaffected as no variables escape.
-- \.0        ~> \.0    --id
-- \.\.1      ~> \.\.1  --const
--
-- Escaping variables are effected.
-- \.1        ~> \.(1+depth)
-- \.\.0 1 2  ~> \.\. 0 1 (2+depth)
--
-- TODO: Refactor code, please..
-- - can probably merge all code into the aux definition/ use mapSubExpr
buryBy :: Binds b => Expr b -> BuryDepth -> Expr b
buryBy expr 0         = expr
buryBy expr buryDepth = case expr of

  Var b
    {--> Var $ intToVar $ (varToInt v) + _unBurryDepth buryDepth-}
    -> Var $ b `addBindIx` (_unBurryDepth buryDepth)

  Lam ty e
    -> Lam ty (buryBy' 0 e buryDepth)

  App f x
    -> App (buryBy f buryDepth) (buryBy x buryDepth)

  Case caseExpr possibleBranches
    -> Case (buryBy caseExpr buryDepth)
            $ case possibleBranches of
                  DefaultOnly defExpr
                    -> DefaultOnly (buryBy defExpr buryDepth)

                  CaseBranches (SomeCaseBranches caseBranch caseBranches) mExpr
                    -> CaseBranches
                        (SomeCaseBranches (mapCaseRHSs (`buryBy` buryDepth) caseBranch) (map (mapCaseRHSs (`buryBy` buryDepth)) caseBranches))
                        ((`buryBy` buryDepth) <$> mExpr)

  Term termName
    -> Term termName

  Sum sumExpr sumIx sumTys
    -> Sum (buryBy sumExpr buryDepth) sumIx sumTys

  Prod prodExprs
    -> Prod (map (`buryBy` buryDepth) prodExprs)

  Union unionExpr tyIx tys
    -> Union (buryBy unionExpr buryDepth) tyIx tys

  where
    buryBy' :: Binds b => Int -> Expr b -> BuryDepth -> Expr b
    buryBy' ourTop expr buryDepth = case expr of

      Var b
        -- Variable is within our height
        | (bindIx b) <= ourTop
          -> Var b

        -- Variable escapes our height, so compensate for the greater depth.
        | otherwise
          -> Var $ b `addBindIx` (_unBurryDepth buryDepth)

      Lam ty e
        -> Lam ty (buryBy' (ourTop+1) e buryDepth)

      App f x
        -> App (buryBy' ourTop f buryDepth) (buryBy' ourTop x buryDepth)

      Case caseExpr possibleBranches
        -> Case (buryBy' ourTop caseExpr buryDepth)
                $ case possibleBranches of
                      DefaultOnly defExpr
                        -> DefaultOnly (buryBy' ourTop defExpr buryDepth)

                      CaseBranches (SomeCaseBranches caseBranch caseBranches) mExpr
                        -> CaseBranches
                            (SomeCaseBranches (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth) caseBranch) (map (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth)) caseBranches))
                            ((\e -> buryBy' ourTop e buryDepth) <$> mExpr)

      Term termName
        -> Term termName

      Sum sumExpr sumIx sumTys
        -> Sum (buryBy' ourTop sumExpr buryDepth) sumIx sumTys

      Prod prodExprs
        -> Prod (map (\e -> buryBy' ourTop e buryDepth) prodExprs)

      Union unionExpr tyIx tys
        -> Union (buryBy' ourTop unionExpr buryDepth) tyIx tys

