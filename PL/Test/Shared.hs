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

   -- Maybe
  , maybeTypeCtx
  , maybeTypeName
  , maybeType
  , maybeSumType
  , nothingTerm
  , justTerm
  , nothingPat
  , justPat

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
import PL.FixPhase
import PL.Kind
import PL.Reduce
import PL.TyVar
import PL.Type
import PL.Type.Eq
import PL.TypeCtx
import PL.Pattern
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
  => TypeCtxFor phase
sharedTypeCtx = mconcat
  [ natTypeCtx
  , boolTypeCtx
  , unitTypeCtx
  , maybeTypeCtx
  , listTypeCtx
  ]

{- Booleans -}

-- | Type context containing the Bool type.
boolTypeCtx
  :: (SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => TypeCtxFor phase
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
  [ EmptyProductT
  , EmptyProductT
  ]

-- | False has type Bool.
falseTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
falseTerm = Sum EmptyProduct 0 boolSumType

-- | True has type Bool.
trueTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,SumTExtension     phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
trueTerm = Sum EmptyProduct 1 boolSumType

-- | Pattern that matches the False value of Bool.
falsePat
  :: (SumPatternExtension phase     ~ Void
     ,ProductPatternExtension phase ~ Void
     )
  => PatternFor phase
falsePat = SumPattern 0 EmptyProductPattern

-- | Pattern that matches the True value of Bool.
truePat
  :: (SumPatternExtension phase     ~ Void
     ,ProductPatternExtension phase ~ Void
     )
  => PatternFor phase
truePat = SumPattern 1 EmptyProductPattern

{- Naturals -}

natTypeCtx
  :: (SumTExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension    phase ~ Void
     )
  => TypeCtxFor phase
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
  [EmptyProductT
  ,Named "Nat"
  ]

zTerm
  :: (SumExtension      phase ~ Void
     ,ProductExtension  phase ~ Void
     ,ProductTExtension phase ~ Void
     ,NamedExtension phase ~ Void
     )
  => ExprFor phase
zTerm = Sum EmptyProduct 0 natSumType

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
  :: (SumPatternExtension     phase ~ Void
     ,ProductPatternExtension phase ~ Void
     )
  => PatternFor phase
zPat = SumPattern 0 EmptyProductPattern

sPat
  :: (SumPatternExtension phase ~ Void)
  => PatternFor phase
  -> PatternFor phase
sPat = SumPattern 1

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
  => TypeCtxFor phase
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
unitTerm = EmptyProduct

-- | Pattern that matches the Unit value of Unit.
unitPat
  :: ProductPatternExtension phase ~ Void
  => PatternFor phase
unitPat = EmptyProductPattern

{- Maybe -}

maybeTypeCtx
  :: (TypeLamExtension phase ~ Void
     ,SumTExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     )
  => TypeCtxFor phase
maybeTypeCtx = fromJust $ insertType "Maybe" maybeType emptyTypeCtx

maybeTypeName
  :: NamedExtension phase ~ Void
  => TypeFor phase
maybeTypeName = Named "Maybe"

maybeType
  :: (TypeLamExtension phase ~ Void
     ,SumTExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     )
  => TypeFor phase
maybeType = TypeLam Kind $ SumT maybeSumType

maybeSumType
  :: (ProductTExtension phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     )
  => NonEmpty (TypeFor phase)
maybeSumType = NE.fromList
  [ EmptyProductT
  , TypeBinding $ TyVar VZ
  ]

nothingTerm
  :: (BigLamExtension phase ~ Void
     ,SumExtension phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     ,ProductExtension phase ~ Void
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
nothingTerm = BigLam Kind $ Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, (TypeBinding $ TyVar VZ)]

justTerm
  :: (BigLamExtension phase ~ Void
     ,LamExtension phase ~ Void
     ,TypeBindingExtension phase ~ Void
     ,TypeBindingFor phase ~ TyVar
     ,SumExtension phase ~ Void
     ,BindingExtension phase ~ Void
     ,BindingFor phase ~ Var
     ,AbstractionFor phase ~ TypeFor phase
     ,ProductTExtension phase ~ Void
     )
  => ExprFor phase
justTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Sum (Binding $ VZ) 1 $ NE.fromList [EmptyProductT, (TypeBinding $ TyVar VZ)]

nothingPat
  :: (SumPatternExtension phase     ~ Void
     ,ProductPatternExtension phase ~ Void
     )
  => PatternFor phase
nothingPat = SumPattern 0 EmptyProductPattern

justPat
  :: (SumPatternExtension phase ~ Void
     ,BindExtension phase ~ Void
     )
  => PatternFor phase
justPat = SumPattern 1 Bind


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
  => TypeCtxFor phase
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
 [ EmptyProductT -- : List a
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
emptyTerm = BigLam Kind $ Sum EmptyProduct 0 listSumType

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
  :: (SumPatternExtension phase     ~ Void
     ,ProductPatternExtension phase ~ Void
     )
  => PatternFor phase
emptyPat = SumPattern 0 EmptyProductPattern

consPat
  :: (SumPatternExtension phase     ~ Void
     ,ProductPatternExtension phase ~ Void
     ,BindExtension phase ~ Void
     )
  => PatternFor phase
consPat = SumPattern 1 (ProductPattern [Bind,Bind])
