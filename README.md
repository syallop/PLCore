# PL - experimental

This package defines the core datastructures and algorithms of a Programming
Language 'designed' according to my whims.

All but core functionality is vastly unfinished. Wherever possible,
compiler-related features have been delegated to external libraries, most of
which have also been written as part of this project.

Some other related components:
- [PLGrammar](https://github.com/syallop/PLGrammar)
- [PLParser](https://github.com/syallop/PLParser)
- [PLPrinter](https://github.com/syallop/PLPrinter)
- [PLLispy](https://github.com/syallop/PLLispy)
- [PLEditor](https://github.com/syallop/PLEditor)
- [PLRepl](https://github.com/syallop/PLRepl)

## Quick-facts

- The core data types currently consist of anonymous functions, sums, products and
  unions.

- Types are structural by default. I.E. if two types have the same structure,
  they can be used interchangeably.

- Mirroring lambda abstraction and function application, types can
  be abstracted over and applied to expressions.

- The type-language is similar to the expression language and permits
  type-variables, application and kind abstraction and application. 

- Variables are bound by de-bruijn indices rather than names. I.E. a variable is
  referenced by the number of lambda abstractions away it is, starting with 0.

- Case analysis must cover all cases. I.E. they are not partial/ do not have
  an undefined case.

- This package does not define or parse any syntax. This makes experimenting
  with syntax easier. Different syntaxes should therefore be able to
  interoperate, perhaps even being translated transparently.  
  - [PLLispy](https://github.com/syallop/PLLispy) is an example syntax that parses and prints expressions
    encapsulated by lisp-like parenthesis. 
  - [PLGrammar](https://github.com/syallop/PLGrammar) could be used to declare syntax that can be interpreted as
    either Parsers or Printers.
    - [PLParser](https://github.com/syallop/PLParser) could be used as the target parser combinator library.
    - [PLPrinter](https://github.com/syallop/PLPrinter) could be used as the targer pretty-printer library.
  
- There is currently no IO. The only computation that can be performed is
  reduction of expressions.

## (partial) Module overview

| Module        | Description |
| ------------- | ----------- |
| PL.Bindings   | When reducing expressions, variables are either unbound (yet) or bound to some applied expression. Variables are referenced by the number of bindings away they appear and so when evaluating under an abstraction indices need to be adjusted carefully. If names were used instead of indices we would need to perform a more complex renaming proceedure. As it is, the API exposes a `bound` `unbound` for adding bindings and `bury` whenever bindings get burried under an abstraction. | 
| PL.Case       | Abstracts case analysis. Case analysis is performed on an expression-like thing using matching-like things. Case analysis must always have a match or a default and should be exhaustive. |
| PL.Error      | Errors that can result from static analysis are captured in a single sum-type and contain typed AST fragments/ references where necessary. A catch-all string like error is provided however most uses should eventually become strongly typed alternatives. |
| PL.Expr       | The core type of expressions is defined to recursively contain itself via FixExpr fixed points. Expressions are abstracted over the choice of binding types, abstraction types and type bindings. Expressions contain lambdas, application, bindings, case analysis, sums, products, unions, type-lambdas and type application. This module can also perform type checking and declares pattern matching constructs. | 
| PL.FixPhase   | A Fixed point over types indexed on their phase in the compilation pipeline. This allows expressions to be recursively nested within themselves, exposing recursion-scheme-like functions for manipulating these nested expressions. |
| PL.Kind       | The type of types is currently either a simple Kind or Type that can be applied to another |
| PL.Name       | Assign distinct Name types to things we wouldn't want to accidentally confuse. |
| PL.Reduce     | Reduce expressions by maintaining a binding ctx and performing substitution and recursive reduction when necessary. |
| PL.Type       | Types inhabited by Expressions. Types are structural until explicitly Named and can abstract and be applied much like Expressions. Types contain named types, arrows, sum types, product types, union types, type arrows, type lambdas, type application and type bindings. Types are abstracted over a binding type and are similarly defined as a fixed point in FixType. |
| PL.TypeCtx    | Maps type names to a definition. Definitions are currently allowed to be declared as recursive or non-recursive (upon themselves). Type declarations are (currently) made out of scope of Exprs |
| PL.Test.\*    | Exports test cases useful for external Parsers to run tests against |

