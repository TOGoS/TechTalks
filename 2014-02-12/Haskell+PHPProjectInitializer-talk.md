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
    - More predictable because all variables need to be explicitly declared and handled
    - Compiler can more easily optimize because it has all information
      at hand and does not need to guarantee evaluation order
    - Interacting with the outside world requires something other than functions
- Currying:
  - No such thing as a zero-argument function
  - Makes things like ```(+ 1) 2``` possible
- Actions (a.k.a. Monads)
  - The part of a Haskell program that _actually do stuff_, since functions can't.
  - ```main``` is an action
  - Separate from functions
  - Actions can call functions and functions can return actions
    - but functions cannot execute actions
    - and actions do not take arguments
    - ```putStrLn``` is a function that returns an action;
      by itself, putStrLn does nothing, and even the action
      it returns does not do anything until it is executed
  - Combine actions with ```>>``` and ```>>=```, or ```do```.

Strict separation of pure/impure world is good idea.
Move as much logic as possible into the pure world.

I like that even though Haskell cleanly separates the pure and impure
parts of a program, programs are still very concise.


## PHP Project Initializer

Since we're all sick of Kohana and I was lacking billable work to do,
Pitt had me come up with recommendations for a new
foundation for new PHP projects, which I have made.

This new foundation consists of:

- a folder structure
- a dependency manager (Composer)
- a bootstrap script
- some custom frameworky classes (PHPCommon)
- a program to initialize a new project (PHP Project Initializer)

It is intentionally non-prescriptive because we can't know right now
what the best tools for a given project at some point in the future will be.

PHP Project Initializer can be updated as we discover what libraries
are helpful and which aren't, possibly adding options for initializing
different kinds of projects (e.g. to leave out everything related to
database connections if an application doesn't need any)

Notes:

- Work so far covers server-side stuff only
- Client-side stuff can fit into the same structure
  - Compile static files directly into www
  - Don't store them in the repository

At this point in the talk, demonstrate setting up a new project with PPI.


### Composer

- It's NPM for PHP
- Define dependencies in ```composer.json```
- Composer downloads and installs them for you
- Give overview of PPI's default ```composer.json```
- We can have private repositories if we need to

### PHPCommon, Nife, and bootstrap.php

Custom bootstrap script based on experience with Schweser, Align projects

Features:

- Configuration, environment initialization scripts are separate from bootstrap.php
  - can be re-used by e.g. command-line programs that need
    the same class loader and/or configuration
- No global variables other than the class loader
  - If you need a separate version of some component, you can make it
- All component initialization is done through a single Registry object
- All web requests are handled through a single Dispatcher object
  - All dispatching logic in one place with very easy-to-follow code.
  - An improvement over the Kohana approach of scattering it through
    various source files (bootstrap, dispatcher, request, controller)!
- Response is encapsulated in a Response object
  - Can write unit tests for your controllers
  - Response can be produced lazily
