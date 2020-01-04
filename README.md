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
λx.x: (0 -> 0)

> \xyz.xyz
λx.λy.λz.x y z: ((4 -> 3) -> ((2 -> 4) -> (2 -> 3)))

> \fxy.f(xy)
λf.λx.λy.f x y: ((4 -> 3) -> ((2 -> 4) -> (2 -> 3)))
```

## Tests

To execute the tests run:

```Bash
stack test
```

## Project Structure

The Project contains a fully working parser for the grammar described abovee, an implementation of the type inference algorithm, and a CLI used for fast and easy type inference checks.