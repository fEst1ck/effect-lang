# Mini-Effect

Mini-Effect is a minimal language that extends the lambda calculus with effect handlers. This repo implements a Mini-Effect interpreter in Typed Racket.

## Syntax

We describe the syntax of programs and types in Mini-Effect.

The core syntax includes the lambda calculus

```text
expr ::= x                    // varaible
      |  (lambda (x) expr)    // abstraction
      |  (expr expr)          // application
```

We extend the lambda calculus with forms for effect handlers.

We have `handler` expressions

```text
expr ::= (handler [(op x k) expr] ...)
```

where `op`, `x`, and `k` are names for the operation, the argument, and the continuation, respectively.

Handlers are used in `with` expressions:

```text
expr ::= (with expr expr)
```

where the first expression evaluates to a handler, under which we evaluate the second expression.

Operations (as opposed to function applications) are marked with `perform`:

```text
expr ::= (perform op arg)
```

where `op` is an operation name and `arg` is an argument expression.

To resume the captured continuation, we use `continue` explicitly:

```text
expr ::= (continue expr)
```

Other built-in forms, operators, and constants are:

```text
expr ::= n                            // numbers: 0, 1, 2, ...
      |  #t                           // true
      |  #f                           // false
      |  s                            // strings: "hello world", "a", "123"
      |  (print expr)                 // print
      |  (< expr expr)                // less than
      |  (= expr expr)                // equal
      |  (+ expr expr)                // addition
      |  (* expr expr)                // multiplication
      | '()                           // empty list
      |  (car expr)                   // car
      |  (cdr expr)                   // cdr
      |  (cons expr expr)             // cons
      |  (if expr expr expr)          // conditional expression
      |  (let [x expr] expr)          // let expression
      |  (do expr ...)                // sequence of expressions
      |  (letrec ([x expr] ...) expr) // recursive let expression
```

A Mini-Effect program is a sequence of definitions or expressions:

```text
top-level ::= (def x expr)
           |  expr

program ::= (top-level ...)
```

We also have types:

```text
op-signature ::= (op . (type ... -> type)) // signature

type ::= (! type (op-signature ...)) // effect type
      |  (-> type type)              // function type
      |  (=> type type)              // effect handler type
```

Optionally, we can type annotate the program with the following syntax:

```text
expr ::= (let [(x : type) expr] expr) // type annotated let expression
      |  ([: expr type] expr)         // type annotated application
      |  (with [: expr type] expr)    // type annotated with expression
```

Type annotations are not required for execution, but are for type checking.

## Semantics of Handlers

We implement the standard semantics of deep effect handlers.

The reduction rule for with expression is:

```text
(with h E[perform op v]) => expr[x = v, k = (with h E[ ])]
```

where the handler `h` handles operation `op` with
`[(op x k) expr]`, and `h` is the inner most handler that handles `op`.

We also support the implicit `return` operation. That is, we have the reduction rule:

```text
(with h E[v]) => expr[x = v]
```

if `h` handles `return` with `[(return x _k) expr]`, and `h` is the inner most handler.

## Meaning of Types

The type `(! T (op . (-> A B)))` means the term produces a value of type `T`, and it may perform operation `op` with arguments of type `(-> A B)`.

The type `(=> C D)` is the type of an effect handler that transforms a computation of type `C` into a computation of type `D`. So suppose a computation `e` has type `C`, and a handler `h` has type `(=> C D)`, then `(with h e)` has type `D`.

## Examples

The `examples/` directory contains example Mini-Effect programs, including all the examples in [1].

## Usage and Examples

To try Mini-Effect, put a `#lang s-exp "mini-effect.rkt"` on top of your file.

```racekt
#lang s-exp "path-to/mini-effect.rkt"

(def reverse
  (handler [(print s k) (do (continue k #f) (print s))]))

(def abc
  (lambda (_)
    (do
      (perform print "A\n")
      (perform print "B\n")
      (perform print "C\n"))))

(with reverse (abc #f))
```

Running the file in DrRacket will show the result in the REPL. Check `examples/` for more examples.

Or you can `(require "lang.rkt")` in a `#lang racket` program, and run `(eval-closed quoted-program)` to evaluate a Mini-Effect program, where quoted-program is a quoted Mini-Effect program.

```racket
> (eval-closed '(with (handler [(get _ k) (continue k 1)]) (perform get ())))
- : Any
1
```

See the unit tests in `lang.rkt` for example usages.

## On the Implementation

We implemented a continuation-passing interpreter (cf. chapter 5 of [3]), which makes the control context of the interpreter explicit. This is necessary to implement effect handlers, which manipulates the continuations.

The core evaluation function has type

```racket
(: eval (-> Term Env Cont Value))
```

The evluation function keeps track of the current computation, an environment, and a continuation.
The environment is a mapping from free variables to values. In `(eval e env cont)`, `env` contains the values of free variables in `e`.
The continuation is a representation of the rest of the computation. And we have `(apply-cont cont v)` to complete the rest of computation when the current computation returns a value `v`.

Our interpreter is essentially a CEK machine [4], since each `(eval e env cont)` only calls `(eval e' env' cont') in a tail position.

For example,

```racket
(eval (+ (f x) 1) env E[ ])
```

would call

```racket
(eval (f x) env E[(+ [] 1)])
```

in a tail position,
which simulates the transition of the state `<(+ (f x) 1), env, E[ ]>` to `<(f x), env, E[(+ [ ] 1)]>` in a CEK machine.

## Outline of the Implementation

Bellow we outline the structure of the implementation.

We implement the interpreter logic in `lang.rkt`. In `lang.rkt`, we first define the data types for environments, continuations, and handlers.

The power of effect handlers comes from its ability to capture a delimited continuation, this is then implemented in

```racket
(: capture-handler (-> Name Cont (Values Cont Cont handler-clause)))
;; op-name: an operation name
;; cont: a continuation
;; If cont = E1[(with-handler h [E2])]
;; where h is the innermost handler that maps op-name to clause
;; returns values: E1, (with-handler h E2), clause
;; Note that (with-handler h E2) is the captured continuation
;; which later binds to the `k` in the handler clause
```

Then comes the core evaluation logic of the interpreter,

```racket
;; Given a continuation and a value, completes the computation.
(: apply-cont (-> Cont Value Value))

;; Evaluates a term in the given environment and continuation.
(: eval (-> Term Env Cont Value))
```

After this, we implement the type checker

```racket
(: type-check (-> Env Term Type Boolean))
;; Checks that in context `ctx`, term `e` has type `type`
;; returns false or raise exceptions if type check fails
```

## Tests

Run unit tests directly with:

```bash
raco test lang.rkt
```

or

```bash
make test
```

## Documentation

Generate the PDF documentation of this file with:

```bash
make pdf
```

## Build

Running `make` will build the PDF documentation and run the unit tests.

## Environment

I tested everything on Racket v8.14. Since I didn't use any fancy features, it should work on any version within the past several years.

## References

[1] M. Pretnar, “An Introduction to Algebraic Effects and Handlers. Invited tutorial paper,” Electronic Notes in Theoretical Computer Science, vol. 319, pp. 19–35, Dec. 2015, doi: 10.1016/j.entcs.2015.12.003.

[2] N. Xie, J. I. Brachthäuser, D. Hillerström, P. Schuster, and D. Leijen, “Effect handlers, evidently,” Proc. ACM Program. Lang., vol. 4, no. ICFP, pp. 1–29, Aug. 2020, doi: 10.1145/3408981.

[3] D. P. Friedman and M. Wand, Essentials of Programming Languages. Cambridge, Ma: Mit Press, 2008.

[4] M. Felleisen and D. P. Friedman, “Control operators, the SECD-machine, and the lambda-calculus,” presented at the Formal Description of Programming Concepts, 1987.
