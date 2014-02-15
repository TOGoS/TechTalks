# Dan's Tech Talk for 2014-02-19

## Haskell

- Why is it called "Haskell"?
  - It's named after Hakell Brooks Curry
  - Brooks and Curry are also named after him,
    and so is the concept of currying (transforming a multi-argument
    function into one that can be called as a chain of functions, each
    taking a single argument, which Haskell does)
- Why talk about Haskell?
  - Ability to write concise code
  - Ability to write efficient code
  - Strict typing and pure functions make code more self-explanatory and
    prevent many errors before they occur
    (Haskell enforces some of the patterns I talked about in my last talk)
- Pure functional
  - Meaning:
    - Functions cannot have side-effects
    - Names cannot be re-bound
    - Data structures are immutable
    - Expressions are referrentially transparent
  - Effects:
    - More predictable because there all variables need to be explicitly declared and handled
    - Compiler can more easily optimize because it has all information
      at hand and does not need to guarantee evaluation order
    - Interacting with the outside world requires something other than functions
- Actions, a.k.a. Monads
  - Separate from functions
  - Actions can call functions and functions can return actions
    but functions cannot execute actions
  - 'main' is an action
  - ```>>``` and ```>>=```
  - mention ```do``` notation

Strict separation of pure/impure world is good idea.
Move as much logic as possible into the pure world.

## The new PHP framework

Since we're all sick of Kohana,
Pitt had me come up with recommendations for a new
foundation for new PHP projects, which I have made.

This new foundation consists of:

- a folder structure
- a dependency manager (Composer)
- a bootstrap script
- a program to initialize a new project

It is intentionally non-prescriptive because we can't know right now
what the best tools for a given project at some point in the future will be.

PHPProjectInitializer can be updated as we discover what libraries
work and which don't, possibly adding options for initializing
different kinds of projects (e.g. to leave out everything related to
database connections if an application doesn't need any)

Notes:

- Work so far covers server-side stuff only
- Client-side stuff can fit into the same structure
  - Compile static files directly into www
  - Don't store them in the repository



TODO: fill in these sectiosn

### Composer

### Nife and the bootstrapper
