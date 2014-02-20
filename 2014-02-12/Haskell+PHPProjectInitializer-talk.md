# Dan's Tech Talk for 2014-02-19

I'm going to talk about a couple of things.
Haskell because I think it's interesting,
and a system for setting up new PHP projects because I want you to use it.

Best time to ask questions is when you have them.

## Haskell

(Write a hello world program)

Given some idle time I decided to learn it because...

- very different than what I'm used to
- safe (compiler will catch most errors before they happen)
- concise syntax that makes certain kinds of operations really easy on the fingers and eyes
  that tend to require a lot more typing in imperative languages
  or even in functionalish OO languages like Ruby or Scala
- most popular (and therefore well-supported) of statically typed functional languages
  - Erlang being a popular dynamically typed one
  - but I like static typing

Features

- Pure functional
  - Meaning:
    - Functions, by definition, do not have side-effects, and given the same inputs,
      will always return the same value 
    - Data structures are immutable
    - Expressions are 'referrentially transparent'
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
    - and actions do not take arguments (like Runnables in Java)
    - ```putStrLn``` is a function that returns an action;
      by itself, putStrLn does nothing, and even the action
      it returns does not do anything until it is executed
  - Combine actions with ```>>``` and ```>>=```, or ```do```.
    (kind of like ```.then(...)``` when using promisey JavaScript)

Strict separation of pure/impure world is good idea
because it makes the behavior of the program better defined
and reduces the number of things that can go wrong.
Move as much logic as possible into the pure world.

I like that even though Haskell cleanly separates the pure and impure
parts of a program, programs are still very concise.

Pitt may ask 'Why is it called "Haskell"?'
- It's named after Hakell Brooks Curry
- Brooks and Curry are also named after him,
  and so is the concept of currying


## PHP Project Initializer

Motivation

Kohana et al give you a monolithic library that's supposed to do everything.

Problem is that they don't do everything and we invariably end up
spending more time working around their limitations than
we saved by using them.

It also results in having large swaths of your codebase
that nobody knows that it's there for.

Even the code that is being used most people don't
completely understand what it does or why it's written the way it is.
They just use it because they think they're supposed to.
This leaves lots of potential for misunderstanding which leads to
bugs and inefficiencies (both in writing and running the code)

But people like the feeling of having somefoundation to build off of,
even if that foundation is made of sugar cubes.  So we keep using this stuff.

Therefore I spent some more idle time
(with encouragement from Pitt and Josh)
to come up with something to start with that:

- does more of what we need out-of-the-box
- stays out of your way when you want to do something different
  (this is accomplished by not overcomplicating things that should be
  simple, like loading configuration files or dispatching requests based
  on URLs or any other parameter)
- is broken into cohesive modules such that you can avoid having the
  pieces that you don't use cluttering up your repository
- can be updated over time as we discover new 'best pratcices'

Rather than write another framework, I've written a program that sets
up a new project following a standard layout and uses Composer to
include a bunch of libraries.

(demonstrate setting up a project)

So what do we have here...

- a folder structure
- a dependency manager (Composer)
- a bootstrap script
- several custom libraries (EarthIT/PHP*)

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

### REST API

Defined in PHPCMIPREST README.

Talk about Composer
Talk about Doctrine
Talk about Postgres

### Composer

- It's NPM for PHP
- Define dependencies in ```composer.json```
- Composer downloads and installs them for you
- Give overview of PPI's default ```composer.json```
- We can have private repositories if we need to

### Doctrine

- Some random database abstraction layer I picked out
- Turning out not to be all that useful
  - can't use identifiers as parameters
  - no notion of last_insert_id???
  - no upsert support

### Postgres

- Sequences are way handier than AUTO_INCREMENT
- Can INSERT ... RETURNING
- Upserts are a bit more verbose than in MySQL but can still be done in a single query
