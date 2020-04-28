{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : PL.Test.Shared
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Shared expressions, types, contexts etc used in tests.
-}
module PL.Test.Shared
  ( sharedTypeCtx

    -- Booleans
  , boolTypeCtx
  , boolTypeName
  , boolType
  , boolSumType
  , falseTerm
  , trueTerm
  , falsePat
  , truePat

    -- Natural numbers
  , natTypeCtx
  , natTypeName
  , natType
  , natSumType
  , zTerm
  , sTerm
  , zPat
  , sPat
  , suc
  , zero
  , one
  , two
  , three
  , four

    -- Unit
  , unitTypeCtx
  , unitTypeName
  , unitType
  , unitProductType
  , unitTerm
  , unitPat

    -- List
  , listTypeCtx
  , listTypeName
  , listType
  , listSumType
  , emptyTerm
  , consTerm
  , emptyPat
  , consPat
  )
  where

import PL.Bindings
import PL.Binds
import PL.Case
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Var

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

sharedTypeCtx
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     ,TypeLamExtension phase ~ Void
     ,TypeAppExtension phase  ~ Void
     )
  => TypeCtx phase
sharedTypeCtx = natTypeCtx <> boolTypeCtx <> unitTypeCtx <> listTypeCtx

{- Booleans -}

-- | Type context containing the Bool type.
boolTypeCtx
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => TypeCtx phase
boolTypeCtx = fromJust $ insertType "Bool" boolType emptyTypeCtx

-- | Name of the Bool type.
boolTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
boolTypeName = Named "Bool"

-- | Unnamed definition of the Bool type.
boolType
  :: (SumTExtension phase ~ Void, ProductTExtension phase ~ Void)
  => TypeFor phase
boolType = SumT boolSumType

-- | Individual types contained in the sum that represents the Bool type.
boolSumType
  :: (SumTExtension phase ~ Void, ProductTExtension phase ~ Void)
  => NonEmpty (TypeFor phase)
boolSumType = NE.fromList $
  [ ProductT []
  , ProductT []
  ]

-- | False has type Bool.
falseTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
falseTerm = Sum (Product []) 0 boolSumType

-- | True has type Bool.
trueTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
trueTerm = Sum (Product []) 1 boolSumType

-- | Pattern that matches the False value of Bool.
falsePat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
falsePat = MatchSum 0 (MatchProduct [])

-- | Pattern that matches the True value of Bool.
truePat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
truePat = MatchSum 1 (MatchProduct [])

{- Naturals -}

natTypeCtx
  :: (SumTExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => TypeCtx phase
natTypeCtx = fromJust $ insertRecType "Nat" natType emptyTypeCtx

natTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
natTypeName = Named "Nat"

natType
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => TypeFor phase
natType = SumT natSumType

natSumType
  :: (ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => NonEmpty (TypeFor phase)
natSumType = NE.fromList $
  [ProductT []
  ,Named "Nat"
  ]

zTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension phase ~ Void
     )
  => ExprFor phase
zTerm = Sum (Product []) 0 natSumType

sTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,ProductTExtension phase ~ Void
     ,LamExtension      phase ~ Void
     ,BindingFor        phase ~ Var
     ,AbstractionFor    phase ~ TypeFor phase
     ,BindingExtension  phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => ExprFor phase
sTerm = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 natSumType

zPat
  :: (MatchSumExtension     phase ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
zPat = MatchSum 0 (MatchProduct [])

sPat
  :: (MatchSumExtension phase ~ Void)
  => MatchArgFor phase
  -> MatchArgFor phase
sPat = MatchSum 1

type SucConstraints phase =
  (AppExtension      phase ~ Void
  ,BindingExtension  phase ~ Void
  ,AbstractionFor    phase ~ TypeFor phase
  ,BindingFor        phase ~ Var
  ,LamExtension      phase ~ Void
  ,ProductExtension  phase ~ Void
  ,ProductTExtension phase ~ Void
  ,SumExtension      phase ~ Void
  ,NamedExtension    phase ~ Void
  )

suc
  :: SucConstraints phase
  => ExprFor phase
  -> ExprFor phase
suc n = App sTerm (n)

zero, one, two, three, four :: SucConstraints phase => ExprFor phase
zero  = zTerm
one   = suc zero
two   = suc one
three = suc two
four  = suc three

{- Unit -}

unitTypeCtx
  :: ProductTExtension phase ~ Void
  => TypeCtx phase
unitTypeCtx = fromJust $ insertType "Unit" unitType emptyTypeCtx

-- | Name of the Unit type.
unitTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
unitTypeName = Named "Unit"

-- | Unnamed definition of the Unit type.
unitType
  :: (ProductTExtension phase ~ Void)
  => TypeFor phase
unitType = ProductT unitProductType

-- | Individual types contained in the product that represents the unit type.
unitProductType
  :: [TypeFor phase]
unitProductType = []

-- | Unit term has type Unit.
unitTerm
  :: ProductExtension phase ~ Void
  => ExprFor phase
unitTerm = Product []

-- | Pattern that matches the Unit value of Unit.
unitPat
  :: MatchProductExtension phase ~ Void
  => MatchArgFor phase
unitPat = MatchProduct []

{- Lists -}
-- TODO: Unfinished/ untested.

listTypeCtx
  :: (TypeLamExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => TypeCtx phase
listTypeCtx = fromJust $ insertRecType "List" listType emptyTypeCtx

listTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
listTypeName = Named "List"

listType
  :: (TypeLamExtension     phase ~ Void
     ,SumTExtension        phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => TypeFor phase
listType = TypeLam Kind $ SumT listSumType

listSumType
  :: (ProductTExtension    phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => NonEmpty (TypeFor phase)
listSumType = NE.fromList $
 [ ProductT [] -- : List a
 , ProductT $ [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
 ]

emptyTerm
  :: (BigLamExtension      phase ~ Void
     ,SumExtension         phase ~ Void
     ,ProductExtension     phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,NamedExtension       phase ~ Void
     )
  => ExprFor phase
emptyTerm = BigLam Kind $ Sum (Product []) 0 listSumType

consTerm
  :: forall phase
   . (BigLamExtension      phase ~ Void
     ,SumExtension         phase ~ Void
     ,ProductExtension     phase ~ Void
     ,TypeAppExtension     phase ~ Void
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ Void
     ,ProductTExtension    phase ~ Void
     ,NamedExtension       phase ~ Void
     ,LamExtension         phase ~ Void
     ,AbstractionFor       phase ~ TypeFor phase
     ,BindingExtension     phase ~ Void
     ,BindingFor           phase ~ Var
     )
  => ExprFor phase
consTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $ Sum (Product [Binding $ VS VZ, Binding VZ]) 1 listSumType

emptyPat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     )
  => MatchArgFor phase
emptyPat = MatchSum 0 (MatchProduct [])

consPat
  :: (MatchSumExtension phase     ~ Void
     ,MatchProductExtension phase ~ Void
     ,BindExtension phase ~ Void
     )
  => MatchArgFor phase
consPat = MatchSum 1 (MatchProduct [Bind,Bind])
