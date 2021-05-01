{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

    -- * Unit
    --
    -- Unit has a single value with no data.
    --
    -- The definition demonstrates:
    -- - Using empty products as types with no data.
    -- - Association of names to type definitions
  , unitTypeCtx
  , unitTypeName
  , unitType
  , unitProductType
  , unitTerm
  , unitPat

    -- * Booleans
    --
    -- Booleans are alternatives of false or true.
    --
    -- Their definition demonstrates:
    -- - Use of sum types as tags
  , boolTypeCtx
  , boolTypeName
  , boolType

  , falseTerm
  , trueTerm

  , falsePat
  , truePat

   -- * Maybe
   --
   -- Maybe is the optionality type. It's values are either Nothing or Just
   -- something.
   --
   -- Their definition demonstrates:
   -- - Types which encapsulate other types by using type variables
  , maybeTypeCtx
  , maybeTypeName
  , maybeType
  , nothingTerm
  , justTerm
  , nothingPat
  , justPat

    -- * Natural numbers
    --
    -- Natural numbers are either zero or the successor of some other natural
    -- number.
    --
    -- Their definition demonstrates:
    -- - Types which refer to themselves
  , natTypeCtx
  , natTypeName
  , natType
  , natTypeDefinition

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

    -- * List
    --
    -- Lists are either Empty or some value and a trailing list of the same type
    -- of value.
    --
    -- Their definition demonstrates:
    -- - Types which refer to themselves AND which have type variables
  , listTypeCtx
  , listTypeName
  , listType
  , listTypeDefinition
  , emptyTerm
  , consTerm
  , emptyPat
  , consPat
  )
  where

import PL.Expr
import PL.FixPhase
import PL.Kind
import PL.TyVar
import PL.Type
import PL.TypeCtx
import PL.Pattern
import PL.Var

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

-- | Associate some common type names to definitions. Includes:
-- - The type of natural numbers 'Nat'
-- - The type of booleans 'Bool'
-- - The unit type 'Unit'
-- - The optionality type 'Maybe'
-- - The list type 'List'.
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
  [ unitTypeCtx
  , boolTypeCtx
  , maybeTypeCtx
  , natTypeCtx
  , listTypeCtx
  ]

{- Booleans -}

-- | Type context containing the Bool type.
boolTypeCtx
  :: ( SumTExtension     phase ~ NoExt
     , ProductTExtension phase ~ NoExt
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

-- | Type context containing the Nat type.
natTypeCtx
  :: ( SumTExtension phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , NamedExtension    phase ~ NoExt
     )
  => TypeCtxFor phase
natTypeCtx = fromJust $ insertRecType "Nat" natTypeDefinition Kind emptyTypeCtx

-- | Name of the Nat type.
natTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
natTypeName = Named "Nat"

-- | Unnamed, recursive definition of the Nat type.
natType
  :: forall phase
   . ( SumTExtension     phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , NamedExtension    phase ~ NoExt
     )
  => TypeFor phase
natType = TypeMu Kind $ natTypeDefinition -- data Nat

-- Bare recursive definition of the Nat type _without_ the enclosing Mu.
natTypeDefinition
  :: forall phase
   . ( SumTExtension     phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , NamedExtension    phase ~ NoExt
     )
  => TypeFor phase
natTypeDefinition = SumT $ NE.fromList $
  [ EmptyProductT   --    = Zero
  , TypeSelfBinding --    | Succ Nat
  ]

-- | Zero has type Nat
-- Z : Nat
zTerm
  :: forall phase
   . ( SumExtension      phase ~ NoExt
     , SumTExtension     phase ~ NoExt
     , ProductExtension  phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , NamedExtension    phase ~ NoExt
     )
  => ExprFor phase
zTerm = Sum EmptyProduct 0 (NE.fromList [EmptyProductT, natTypeName])

-- | Successor has type Nat -> Nat
-- S n : Nat
sTerm
  :: ( SumExtension      phase ~ NoExt
     , SumTExtension     phase ~ NoExt
     , ProductExtension  phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , LamExtension      phase ~ NoExt
     , BindingFor        phase ~ Var
     , AbstractionFor    phase ~ TypeFor phase
     , BindingExtension  phase ~ NoExt
     , NamedExtension    phase ~ NoExt
     )
  => ExprFor phase
sTerm = Lam (Named "Nat") $ Sum (Binding (mkVar 0)) 1 (NE.fromList [EmptyProductT, natTypeName])

-- | Pattern that matches the Zero value of Nat.
zPat
  :: ( SumPatternExtension     phase ~ NoExt
     , ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
zPat = SumPattern 0 EmptyProductPattern

-- | Pattern that matches successor of a Nat pattern.
sPat
  :: (SumPatternExtension phase ~ NoExt)
  => PatternFor phase
  -> PatternFor phase
sPat = SumPattern 1

type SucConstraints phase =
  ( AppExtension      phase ~ NoExt
  , BindingExtension  phase ~ NoExt
  , AbstractionFor    phase ~ TypeFor phase
  , BindingFor        phase ~ Var
  , LamExtension      phase ~ NoExt
  , ProductExtension  phase ~ NoExt
  , ProductTExtension phase ~ NoExt
  , SumTExtension     phase ~ NoExt
  , SumExtension      phase ~ NoExt
  , NamedExtension    phase ~ NoExt
  )

-- | Layer a Succ constructor around a Nat.
suc
  :: SucConstraints phase
  => ExprFor phase
  -> ExprFor phase
suc n = App sTerm (n)

-- | Convenience for some natural numbers. Use suc to create more.
zero, one, two, three, four :: SucConstraints phase => ExprFor phase
zero  = zTerm
one   = suc zero
two   = suc one
three = suc two
four  = suc three

{- Unit -}

-- | Type context containing the Unit type.
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

-- | Type context containing the Maybe type.
maybeTypeCtx
  :: ( TypeLamExtension phase ~ NoExt
     , SumTExtension phase ~ NoExt
     , ProductTExtension phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor phase ~ TyVar
     )
  => TypeCtxFor phase
maybeTypeCtx = fromJust $ insertType "Maybe" maybeType (KindArrow Kind Kind) emptyTypeCtx

-- | Name of the Maybe type.
maybeTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
maybeTypeName = Named "Maybe"

-- | Unnamed definition of the Maybe type.
maybeType
  :: ( TypeLamExtension     phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     )
  => TypeFor phase
maybeType = TypeLam Kind $ SumT $ NE.fromList -- data Maybe a
  [ EmptyProductT                             --    = Nothing
  , TypeBinding $ TyVar VZ                    --    | Just a
  ]

-- | Nothing accepts a type a to produce a value Maybe a
-- nothing : forall a. Maybe a
nothingTerm
  :: ( BigLamExtension      phase ~ NoExt
     , SumExtension         phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , ProductExtension     phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     )
  => ExprFor phase
nothingTerm = BigLam Kind $ Sum EmptyProduct 0 $ NE.fromList
  [ EmptyProductT
  , (TypeBinding $ TyVar VZ)
  ]

-- | Just accepts a type and a value of that type in order to produce a value
-- Maybe a.
--
-- Just : forall a. a -> Maybe a
justTerm
  :: ( BigLamExtension      phase ~ NoExt
     , LamExtension         phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , SumExtension         phase ~ NoExt
     , BindingExtension     phase ~ NoExt
     , BindingFor           phase ~ Var
     , AbstractionFor       phase ~ TypeFor phase
     , ProductTExtension    phase ~ NoExt
     )
  => ExprFor phase
justTerm = BigLam Kind $ Lam (TypeBinding $ TyVar VZ) $ Sum (Binding $ VZ) 1 $ NE.fromList
  [ EmptyProductT
  , TypeBinding $ TyVar VZ
  ]

-- | Pattern that matches the Nothing value of Maybe.
nothingPat
  :: ( SumPatternExtension     phase ~ NoExt
     , ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
nothingPat = SumPattern 0 EmptyProductPattern

-- | Pattern that binds the Just value of Maybe.
justPat
  :: ( SumPatternExtension phase ~ NoExt
     , BindExtension phase ~ NoExt
     )
  => PatternFor phase
justPat = SumPattern 1 Bind


{- Lists -}
-- TODO: Unfinished/ untested.

-- | Type context containing the List type.
listTypeCtx
  :: ( TypeLamExtension     phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , TypeBindingExtension phase ~ NoExt
     , TypeAppExtension     phase ~ NoExt
     , NamedExtension       phase ~ NoExt
     )
  => TypeCtxFor phase
listTypeCtx = fromJust $ insertRecType "List" listTypeDefinition (KindArrow Kind Kind) emptyTypeCtx

-- | Name of the List type.
listTypeName
  :: NamedExtension phase ~ NoExt
  => TypeFor phase
listTypeName = Named "List"

-- | Unnamed, recursive definition of the List type.
listType
  :: ( TypeLamExtension     phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , TypeAppExtension     phase ~ NoExt
     , NamedExtension       phase ~ NoExt
     )
  => TypeFor phase
listType = TypeMu (KindArrow Kind Kind) $ listTypeDefinition -- data List ...

-- | Bare recursive definition of the List type _without_ the enclosing Mu.
listTypeDefinition
  :: ( TypeLamExtension     phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , TypeBindingExtension phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , TypeAppExtension     phase ~ NoExt
     , NamedExtension       phase ~ NoExt
     )
  => TypeFor phase
listTypeDefinition = TypeLam Kind $ SumT $ NE.fromList $       -- ... a
 [ EmptyProductT                                               --    = Empty
 , ProductT [ TypeBinding $ TyVar VZ                           --    | Cons a
            , TypeApp TypeSelfBinding (TypeBinding $ TyVar VZ) --           List a
            ]
 ]


-- | The empty term takes a type to produce a List.
emptyTerm
  :: ( BigLamExtension      phase ~ NoExt
     , SumExtension         phase ~ NoExt
     , ProductExtension     phase ~ NoExt
     , TypeAppExtension     phase ~ NoExt
     , TypeLamExtension     phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , TypeBindingExtension phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , NamedExtension       phase ~ NoExt
     )
  => ExprFor phase
emptyTerm = BigLam Kind $ Sum EmptyProduct 0 $ NE.fromList
  [ EmptyProductT
  , ProductT [ TypeBinding $ TyVar VZ
             , TypeApp listTypeName (TypeBinding $ TyVar VZ)
             ]
  ]

-- | The cons term takes a type of contained values, a value for the head, then
-- a list of values for the tail.
--
-- Cons : forall a. a -> [a] -> [a]
consTerm
  :: forall phase
   . ( BigLamExtension      phase ~ NoExt
     , SumExtension         phase ~ NoExt
     , ProductExtension     phase ~ NoExt
     , SumTExtension        phase ~ NoExt
     , TypeAppExtension     phase ~ NoExt
     , TypeBindingFor       phase ~ TyVar
     , TypeBindingExtension phase ~ NoExt
     , ProductTExtension    phase ~ NoExt
     , NamedExtension       phase ~ NoExt
     , LamExtension         phase ~ NoExt
     , TypeLamExtension     phase ~ NoExt
     , AbstractionFor       phase ~ TypeFor phase
     , BindingExtension     phase ~ NoExt
     , BindingFor           phase ~ Var
     )
  => ExprFor phase
consTerm = BigLam Kind                                         -- forall a.
         $ Lam (TypeBinding $ TyVar VZ)                        -- \(x  : a)
         $ Lam (TypeApp listTypeName (TypeBinding $ TyVar VZ)) -- \(xs : [a])
          $ Sum                                                -- -> Cons
                (Product [ Binding $ VS VZ                     --      x
                         , Binding VZ                          --      xs
                         ]
                )
                1
                (NE.fromList                                   -- : ...
                  [ EmptyProductT
                  , ProductT [ TypeBinding $ TyVar VZ
                             , TypeApp listType (TypeBinding $ TyVar VZ)
                             ]
                  ]
                )

-- | Pattern that matches the Empty value of a List.
emptyPat
  :: ( SumPatternExtension phase     ~ NoExt
     , ProductPatternExtension phase ~ NoExt
     )
  => PatternFor phase
emptyPat = SumPattern 0 EmptyProductPattern

-- | Pattern that matches the head and tail of a List.
consPat
  :: ( SumPatternExtension phase     ~ NoExt
     , ProductPatternExtension phase ~ NoExt
     , BindExtension phase ~ NoExt
     )
  => PatternFor phase
consPat = SumPattern 1 (ProductPattern [Bind, Bind])

