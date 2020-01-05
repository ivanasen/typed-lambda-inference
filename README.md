# Typed Lambda Inference

A Haskell implementation of the [Hindley-Milner Type Inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) algorithm for Simply Typed Lambda Calculus.

This repository contains a parser for the grammar described below, an implementation of the type inference algorithm, and a REPL which can be used to play around with the algorithm.

This project was made as homework for the Functional Programming course at Sofia University "St. Kliment Ohridski".


# Prerequisites

- A working **Stack** installation.

## Usage

The project ships with an interactive Read-Eval-Print-Loop (REPL) that you can use to infer lambda expressions. To build and run the REPL, you can run the command:

```bash
stack run
```

The lambda expressions which the REPL can accept and infer are described by the abstract grammar:

```Haskell
E ::= V         -- Variable
    | \A.E      -- Abstraction
    | E E       -- Application
    | (E)

A ::= VA | V    -- Arguments

V ::= a         -- Identifier
      where a <- Unicode \ {'.', '\', '(', ')'}
```

### Example inputs and outputs:

```Bash
> \x.x
λx.x: (A -> A)

> \xyz.xyz
λx.λy.λz.((x y) z): ((B -> (C -> D)) -> (B -> (C -> D))

> \fxy.f(xy)
λf.λx.λy.(f (x y)): ((E -> D) -> ((C -> E) -> (C -> D)))
```

## Tests

To execute the tests run:

```Bash
stack test
```

## Used Resources
- [f(by) 2019 - Christoph Hegemann, TYPE INFERENCE FROM SCRATCH](https://www.youtube.com/watch?v=ytPAlhnAKro)
- [Type Inference - For the Simply-Typed Lambda Calculus](https://proglang.informatik.uni-freiburg.de/teaching/compilerbau/2012ws/17-simply-typed.pdf)
- [Lambda Calculus articles at crypto.stanford.edu](https://crypto.stanford.edu/~blynn/lambda/)