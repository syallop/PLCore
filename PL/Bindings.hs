{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
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

  , buryN
  , bindingsFromList
  , append
  ) where


import Control.Applicative

import PL.Expr
import PL.Binds

-- | A positive integer indicating how many lambda abstractions a bound expression has been moved under.
newtype BuryDepth = BuryDepth {_unBurryDepth :: Int} deriving (Num,Eq)

-- A context of bound 'Expr b abs' in which you may:
-- - 'bind'    : E.G. when an expr is Applied to a Lambda.
-- - 'unbound' : E.G. when you want to manipulate expressions under a Lambda abstraction whose binding hasnt been applied yet.
-- - 'bury'    : E.G. when all of the bindings are lifted under an abstraction, any escaping bindings should be burried.
data Bindings b abs
  = EmptyBindings                        -- ^ No bindings
  | ConsBinding (Binding b abs) (Bindings b abs) -- ^ A new binding
  | Buried (Bindings b abs)                  -- ^ Bury many bindings beneath a lambda abstraction
  deriving Show

data Binding b abs
  = Unbound            -- No expression bound yet
  | Bound (Expr b abs) -- This expression is bound
  deriving Show

emptyBindings :: Bindings b abs
emptyBindings = EmptyBindings

-- | Bury bindings under a lambda abstraction
bury :: Bindings b abs -> Bindings b abs
bury = Buried

buryN :: Int -> Bindings b abs -> Bindings b abs
buryN 0 b = b
buryN n b = buryN (n-1) (Buried b)

-- | Introduce an unbound binding
unbound :: Bindings b abs -> Bindings b abs
unbound = ConsBinding Unbound

-- | Introduce a new bound binding
bind :: Expr b abs -> Bindings b abs -> Bindings b abs
bind = ConsBinding . Bound

-- append a list of bound expressions to a bindings
append :: [Expr b abs] -> Bindings b abs -> Bindings b abs
append []     bs = bs
append (x:xs) bs = ConsBinding (Bound x) (append xs bs)

-- | Transform a list of expressions to bind into a bindings
bindingsFromList :: [Expr b abs] -> Bindings b abs
bindingsFromList []     = EmptyBindings
bindingsFromList (b:bs) = ConsBinding (Bound b) (bindingsFromList bs)

-- | If the index exists, extract the bound expression, itself adjusted for it's depth in the bindings.
safeIndex :: BindAbs b abs => Bindings b abs -> Int -> Maybe (Binding b abs)
safeIndex EmptyBindings _ = Nothing
safeIndex bs           ix = safeIndex' 0 bs ix
  where
    safeIndex' :: BindAbs b abs => BuryDepth -> Bindings b abs -> Int -> Maybe (Binding b abs)
    safeIndex' buryDepth bs ix = case (bs,ix) of
      (EmptyBindings    , _) -> Nothing

      (ConsBinding b _  , 0) -> case b of
                                   Unbound    -> Just Unbound
                                   Bound expr -> Just $ Bound $ expr `buryBy` buryDepth
      (ConsBinding _ bs', _) -> safeIndex' buryDepth     bs' (ix-1)

      (Buried        bs', _) -> safeIndex' (buryDepth+1) bs' ix

-- | 'safeIndex' assuming the index is contained in the bindings.
index :: BindAbs b abs => Bindings b abs -> Int -> Binding b abs
index bs ix = let Just r = safeIndex bs ix in r

-- bury any escaping bindings in an expression by given depth.
--
-- E.G.
-- Unaffected as no bindings escape.
-- \.0        ~> \.0    --id
-- \.\.1      ~> \.\.1  --const
--
-- Escaping bindings are effected.
-- \.1        ~> \.(1+depth)
-- \.\.0 1 2  ~> \.\. 0 1 (2+depth)
--
-- TODO: Refactor code, please..
-- - can probably merge all code into the aux definition/ use mapSubExpr
buryBy :: BindAbs b abs => Expr b abs -> BuryDepth -> Expr b abs
buryBy expr 0         = expr
buryBy expr buryDepth = case expr of

  Binding b
    -> Binding $ buryBinding b (_unBurryDepth buryDepth)

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

  Sum sumExpr sumIx sumTys
    -> Sum (buryBy sumExpr buryDepth) sumIx sumTys

  Product productExprs
    -> Product $ map (`buryBy` buryDepth) productExprs

  Union unionExpr tyIx tys
    -> Union (buryBy unionExpr buryDepth) tyIx tys

  where
    buryBy' :: BindAbs b abs => Int -> Expr b abs -> BuryDepth -> Expr b abs
    buryBy' ourTop expr buryDepth = case expr of

      Binding b
        -- Binding is within our height
        | bindDepth b <= ourTop
          -> Binding b

        -- Binding escapes our height, so compensate for the greater depth.
        | otherwise
          -> Binding $ b `buryBinding` (_unBurryDepth buryDepth)

      Lam abs e
        -> Lam abs (buryBy' (ourTop+1) e buryDepth)

      App f x
        -> App (buryBy' ourTop f buryDepth) (buryBy' ourTop x buryDepth)

      Case caseExpr possibleBranches
        -> Case (buryBy' ourTop caseExpr buryDepth)
                $ case possibleBranches of
                      DefaultOnly defExpr
                        -> DefaultOnly (buryBy' ourTop defExpr buryDepth)

                      -- TODO LHS which contain MatchBinding's should probably be buried
                      CaseBranches (SomeCaseBranches caseBranch caseBranches) mExpr
                        -> CaseBranches
                            (SomeCaseBranches (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth) caseBranch) (map (mapCaseRHSs (\e -> buryBy' ourTop e buryDepth)) caseBranches))
                            ((\e -> buryBy' ourTop e buryDepth) <$> mExpr)

      Sum sumExpr sumIx sumTys
        -> Sum (buryBy' ourTop sumExpr buryDepth) sumIx sumTys

      Product productExprs
        -> Product $ map (\e -> buryBy' ourTop e buryDepth) productExprs

      Union unionExpr tyIx tys
        -> Union (buryBy' ourTop unionExpr buryDepth) tyIx tys

