{-|
Module      : PL
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

Re-Exports components of this 'PL' Programming Language, a language that starts
with anonymous functions, sums, products and unions types which will be extended
whimsically.
-}
module PL where

import PL.Bindings           as X
import PL.Binds              as X
import PL.Binds.Ix           as X

-- Case analysis on... things
import PL.Case               as X

-- Hash things to produce Content-Addressed names
import PL.Hash               as X

-- Expressions, Types, etc extended with comments.
import PL.Commented          as X

-- Errors that may be thrown in various compilation stages.
import PL.Error              as X

-- An AST containing anonymous functions, sums, products and union types.
-- Indexed by de bruijn indexes and with some level of type functions.
import PL.Expr               as X

-- Classes of things which are like expressions. Types and kinds have a similar
-- structure.
import PL.ExprLike           as X

-- The Type of Types.
import PL.Kind               as X

-- Assign new name types to things we really wouldnt want to accidentally confuse.
-- Type and variable names for example.
import PL.Name               as X

-- A description of a languages grammar. Can be translated to a Parser and
-- eventually to a corresponing printer.
import PLGrammar             as X

-- A NIH Pretty-Printer
import PLPrinter              as X

-- Machinery for representing ASTs as fix points over their recursive
-- occurances, with a 'phase' parameter to accomodate a trees-that-grow pattern.
import PL.FixPhase            as X

-- Reduce expressions by maintaining a binding ctx and performing substitution
-- and recursive reduction when necessary.
import PL.Reduce             as X

-- Duplication of Reduce but acting at the type level. Currently has the right
-- to behave differently and terminate on types that could be otherwise reduced.
import PL.ReduceType         as X

-- Interface for storing arbitrary key-values
import PL.Store              as X

-- Type-level variables which can be used with a binding context to Kinds.
import PL.TyVar              as X

-- Types inhabited by Expressions. Types are structural until explicitly Named
-- and can abstract and be applied much like Expressions.
import PL.Type               as X

-- Maps names to types allowing type resolution.
import PL.TypeCtx            as X

-- Variables which can be used within a binding context to Types.
import PL.Var

