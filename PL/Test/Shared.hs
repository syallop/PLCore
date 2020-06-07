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
  :: (SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,NamedExtension    phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     ,TypeLamExtension phase ~ NoExt
     ,TypeAppExtension phase  ~ NoExt
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
  :: (SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => TypeCtxFor phase
boolTypeCtx = fromJust $ insertType "Bool" boolType Kind emptyTypeCtx

-- | Name of the Bool type.
boolTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
boolTypeName = Named "Bool"

-- | Unnamed definition of the Bool type.
boolType
  :: (SumTExtension phase ~ NoExt, ProductTExtension phase ~ NoExt)
  => TypeFor phase
boolType = SumT boolSumType

-- | Individual types contained in the sum that represents the Bool type.
boolSumType
  :: (SumTExtension phase ~ NoExt, ProductTExtension phase ~ NoExt)
  => NonEmpty (TypeFor phase)
boolSumType = NE.fromList $
  [ EmptyProductT
  , EmptyProductT
  ]

-- | False has type Bool.
falseTerm
  :: (SumExtension      phase ~ NoExt
     ,ProductExtension  phase ~ NoExt
     ,SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => ExprFor phase
falseTerm = Sum EmptyProduct 0 boolSumType

-- | True has type Bool.
trueTerm
  :: (SumExtension      phase ~ NoExt
     ,ProductExtension  phase ~ NoExt
     ,SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => ExprFor phase
trueTerm = Sum EmptyProduct 1 boolSumType

-- | Pattern that matches the False value of Bool.
falsePat
  :: (SumPatternExtension phase     ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
falsePat = SumPattern 0 EmptyProductPattern

-- | Pattern that matches the True value of Bool.
truePat
  :: (SumPatternExtension phase     ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
truePat = SumPattern 1 EmptyProductPattern

{- Naturals -}

natTypeCtx
  :: (SumTExtension phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,NamedExtension    phase ~ NoExt
     )
  => TypeCtxFor phase
natTypeCtx = fromJust $ insertRecType "Nat" natType Kind emptyTypeCtx

natTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
natTypeName = Named "Nat"

natType
  :: (SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,NamedExtension    phase ~ NoExt
     )
  => TypeFor phase
natType = SumT natSumType

natSumType
  :: (ProductTExtension phase ~ NoExt
     ,NamedExtension    phase ~ NoExt
     )
  => NonEmpty (TypeFor phase)
natSumType = NE.fromList $
  [EmptyProductT
  ,Named "Nat"
  ]

zTerm
  :: (SumExtension      phase ~ NoExt
     ,ProductExtension  phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,NamedExtension phase ~ NoExt
     )
  => ExprFor phase
zTerm = Sum EmptyProduct 0 natSumType

sTerm
  :: (SumExtension      phase ~ NoExt
     ,ProductExtension  phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,LamExtension      phase ~ NoExt
     ,BindingFor        phase ~ Var
     ,AbstractionFor    phase ~ TypeFor phase
     ,BindingExtension  phase ~ NoExt
     ,NamedExtension    phase ~ NoExt
     )
  => ExprFor phase
sTerm = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 natSumType

zPat
  :: (SumPatternExtension     phase ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
zPat = SumPattern 0 EmptyProductPattern

sPat
  :: (SumPatternExtension phase ~ NoExt)
  => PatternFor phase
  -> PatternFor phase
sPat = SumPattern 1

type SucConstraints phase =
  (AppExtension      phase ~ NoExt
  ,BindingExtension  phase ~ NoExt
  ,AbstractionFor    phase ~ TypeFor phase
  ,BindingFor        phase ~ Var
  ,LamExtension      phase ~ NoExt
  ,ProductExtension  phase ~ NoExt
  ,ProductTExtension phase ~ NoExt
  ,SumExtension      phase ~ NoExt
  ,NamedExtension    phase ~ NoExt
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
  :: ProductTExtension phase ~ NoExt
  => TypeCtxFor phase
unitTypeCtx = fromJust $ insertType "Unit" unitType Kind emptyTypeCtx

-- | Name of the Unit type.
unitTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
unitTypeName = Named "Unit"

-- | Unnamed definition of the Unit type.
unitType
  :: (ProductTExtension phase ~ NoExt)
  => TypeFor phase
unitType = ProductT unitProductType

-- | Individual types contained in the product that represents the unit type.
unitProductType
  :: [TypeFor phase]
unitProductType = []

-- | Unit term has type Unit.
unitTerm
  :: ProductExtension phase ~ NoExt
  => ExprFor phase
unitTerm = EmptyProduct

-- | Pattern that matches the Unit value of Unit.
unitPat
  :: ProductPatternExtension phase ~ NoExt
  => PatternFor phase
unitPat = EmptyProductPattern

{- Maybe -}

maybeTypeCtx
  :: (TypeLamExtension phase ~ NoExt
     ,SumTExtension phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     )
  => TypeCtxFor phase
maybeTypeCtx = fromJust $ insertType "Maybe" maybeType (KindArrow Kind Kind) emptyTypeCtx

maybeTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
maybeTypeName = Named "Maybe"

maybeType
  :: (TypeLamExtension phase ~ NoExt
     ,SumTExtension phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     )
  => TypeFor phase
maybeType = TypeLam Kind $ SumT maybeSumType

maybeSumType
  :: (ProductTExtension phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     )
  => NonEmpty (TypeFor phase)
maybeSumType = NE.fromList
  [ EmptyProductT
  , TypeBinding $ TyVar VZ
  ]

nothingTerm
  :: (BigLamExtension phase ~ NoExt
     ,SumExtension phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     ,ProductExtension phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     )
  => ExprFor phase
nothingTerm = BigLam Kind $ Sum EmptyProduct 0 $ NE.fromList [EmptyProductT, (TypeBinding $ TyVar VZ)]

justTerm
  :: (BigLamExtension phase ~ NoExt
     ,LamExtension phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor phase ~ TyVar
     ,SumExtension phase ~ NoExt
     ,BindingExtension phase ~ NoExt
     ,BindingFor phase ~ Var
     ,AbstractionFor phase ~ TypeFor phase
     ,ProductTExtension phase ~ NoExt
     )
  => ExprFor phase
justTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Sum (Binding $ VZ) 1 $ NE.fromList [EmptyProductT, (TypeBinding $ TyVar VZ)]

nothingPat
  :: (SumPatternExtension phase     ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
nothingPat = SumPattern 0 EmptyProductPattern

justPat
  :: (SumPatternExtension phase ~ NoExt
     ,BindExtension phase ~ NoExt
     )
  => PatternFor phase
justPat = SumPattern 1 Bind


{- Lists -}
-- TODO: Unfinished/ untested.

listTypeCtx
  :: (TypeLamExtension  phase ~ NoExt
     ,SumTExtension     phase ~ NoExt
     ,ProductTExtension phase ~ NoExt
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ NoExt
     ,TypeAppExtension     phase ~ NoExt
     ,NamedExtension       phase ~ NoExt
     )
  => TypeCtxFor phase
listTypeCtx = fromJust $ insertRecType "List" listType (KindArrow Kind Kind) emptyTypeCtx

listTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
listTypeName = Named "List"

listType
  :: (TypeLamExtension     phase ~ NoExt
     ,SumTExtension        phase ~ NoExt
     ,ProductTExtension    phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ NoExt
     ,NamedExtension       phase ~ NoExt
     )
  => TypeFor phase
listType = TypeLam Kind $ SumT listSumType

listSumType
  :: (ProductTExtension    phase ~ NoExt
     ,TypeBindingExtension phase ~ NoExt
     ,TypeBindingFor       phase ~ TyVar
     ,TypeAppExtension     phase ~ NoExt
     ,NamedExtension       phase ~ NoExt
     )
  => NonEmpty (TypeFor phase)
listSumType = NE.fromList $
 [ EmptyProductT -- : List a
 , ProductT $ [TypeBinding $ TyVar VZ, TypeApp listTypeName (TypeBinding $ TyVar VZ)]
 ]

emptyTerm
  :: (BigLamExtension      phase ~ NoExt
     ,SumExtension         phase ~ NoExt
     ,ProductExtension     phase ~ NoExt
     ,TypeAppExtension     phase ~ NoExt
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ NoExt
     ,ProductTExtension    phase ~ NoExt
     ,NamedExtension       phase ~ NoExt
     )
  => ExprFor phase
emptyTerm = BigLam Kind $ Sum EmptyProduct 0 listSumType

consTerm
  :: forall phase
   . (BigLamExtension      phase ~ NoExt
     ,SumExtension         phase ~ NoExt
     ,ProductExtension     phase ~ NoExt
     ,TypeAppExtension     phase ~ NoExt
     ,TypeBindingFor       phase ~ TyVar
     ,TypeBindingExtension phase ~ NoExt
     ,ProductTExtension    phase ~ NoExt
     ,NamedExtension       phase ~ NoExt
     ,LamExtension         phase ~ NoExt
     ,AbstractionFor       phase ~ TypeFor phase
     ,BindingExtension     phase ~ NoExt
     ,BindingFor           phase ~ Var
     )
  => ExprFor phase
consTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) $ Sum (Product [Binding $ VS VZ, Binding VZ]) 1 listSumType

emptyPat
  :: (SumPatternExtension phase     ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
emptyPat = SumPattern 0 EmptyProductPattern

consPat
  :: (SumPatternExtension phase     ~ NoExt
     ,ProductPatternExtension phase ~ NoExt
     ,BindExtension phase ~ NoExt
     )
  => PatternFor phase
consPat = SumPattern 1 (ProductPattern [Bind,Bind])
