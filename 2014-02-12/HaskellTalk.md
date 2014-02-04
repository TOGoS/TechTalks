# Introductory Notes on Haskell

I had run out of things to do for First30 and decided it was time to
learn Haskell because I'd heard nice things about it.  Pitt said I
should make a talk about it.

I haven't yet written any useful programs in Haskell so I can't give
an informed opinion about its strengths or weaknesses for large
projects, so instead I'll just give an overview of the language and
talk about how it differs from imperative languages that most of us
are used to.

## Language overview

#### Actions

To get started, here's Hello, world in Haskell:

```haskell
main :: IO ()
main = putStrLn "Hello, world!"
```

Unless you've programmed in Haskell or a Haskell-like language before,
this is probably Greek to you.

In order to explain what's going on here I need to explain that
Haskell is a purely functional language, which means that functions
are not allowed to have side-effects.  Which may lead you to ask
"How can your program print 'Hello, world!' if no side-effects are allowed?".
The answer is that while ~functions~ may not have side-effects, Haskell
has a notion of 'actions', separate from functions, which /do/ have side-effects.

The type for 'action' is spelled 'IO <return value type>', where "()"
is used to indicate no result, hence, in our example, 'main' being of
type 'IO ()'.  Main is an action that has side effects but doesn't
return anything.  We're defining main as the result of calling
putStrLn with a single argument, the literal string "Hello, world!",
putStrLn being a function that doesn't actually output anything on its
own, but that returns an action that, when run, will output "Hello,
world!"

So to summarize...

```haskell
-- Main's type is 'action that returns nothing'
main :: IO ()

-- Main is the result of calling (putStrLn "Hello, world!")
main = putStrLn "Hello, world!"
```

#### Operators

Next I will talk about some of Haskell's operators that make Haskell
programs appear incomprehensible until you know what they mean.

```haskell
>>
```

Is used to combine actions.  It means 'action on the left, followed by action on the right'.
If we wanted our 'main' action to do more than one thing, we could use this:

```haskell
main =
  putStrLn "I shall write this line, and then another!" >>
  putStrLn "This is the second line."
```

Relatedly,

```haskell
>>=
```

Is used to combine an action with a function that takes the action's
result and returns another action.  We would use this if we wanted to
take some input and then act on it:

```haskell
main = getLine >>= putStrLn
```

Indicates that main is an action that reads a line from standard input
and passes the result of getLine (which happens to be a string) to
putStrLn, and then run the action that putStrLn returns.  i.e.
main will read a line and then spit it back at you.

Haskell provides another way to write action chains, 'do' blocks:

```haskell
main = do
  l <- getLine
  putStrLn l
```

This is interpreted like:

```haskell
main =
  getLine >>=
  (\l -> putStrLn l)
```

Where (\l -> ...) defines an anonymous function that takes 'l' and passes it to putStrLn.
Some people say the 'do' notation is harmful because it obscures this, and
at this point feel like I agree with that sentiment.

```haskell
$
```

The dollar operator applies a function.
Since this has lower precedence than a space,
people sometimes like to use this to cut down on parentheses.
Example:

```haskell
some function call (another function call)
```

Can be rewritten as:

```haskell
some function call $ another function call
```

For this to make sense you must realize that in Haskell,
all functions are automatically curried, so

```haskell
(\b -> someFunction 12 b)
```
  
Can be more succinctly written as

```haskell
someFunction 12
```

And

```haskell
foo bar baz
```

Has the exact same meaning as

```haskell
(((foo) bar) baz)
```

Infix operators are automatically curried, too, so you can do things like:

```haskell
(1 +) 5
(/ 3) 12
```

Given that ++ is the string (also list) concatenation operator,
you could make a greeting generator function like this:

```haskell
greet = ("Hello, " ++)
greet "John"
< "Hello, John"
```

Haskell also has a handy function composition operator, ".", where

```haskell
(f . g) x
```

Is equivalent to

```haskell
f (g x)
```

Or, if we like dollar signs,

```haskell
f $ g x
```

That might not seem particularly useful in cases where we immediatly call
the functions, but it's a nice shorthand when we want to define a function for
use later.

As a somehwat contrived example, let's say we want to extend our greet function
so that it adds an exclamation point on the end.  One way to do it would be to
use a named argument like this:

```haskell
greet name = "Hello, " ++ name ++ "!"
```

Or:

```haskell
greet = (\name -> "Hello, " ++ name ++ "!")
```

But you also could think of it as a chain of operations:

1. Append "!"
2. Prepend with "Hello, "

This is exactly what . does:

```haskell
greet = ("Hello " ++) . (++ "!")
greet "Fred"
< "Hello, Fred!"
```

#### Data structures

TODO

#### Pattern matching

TODO

#### Lazy evaluation

TODO

## Things to watch out for

The first hurdle I had to overcome when learning Haskell was that
you have to define things slightly differently in ghci

## What do I think of it?

It's a nice language, but it takes some getting used to,
especially if you're already used to imperative languages.

Once you understand precedence and what operators do,
writing code in Haskell is pretty painless and I find that
once I get a piece of code to compile, it works as expected,
and that code's a lot shorter than what I would write in some
imperative language like PHP or Java.

So the advantages of Haskell are:

- Write less code
- Have fewer bugs
- Decent performance for high-level programs

And the primary disadvantages of Haskell are:

- It requires re-training
- Getting good performance in tight loops may require some tweaking

Since we're primarily a client-side web application shop,
the bulk of our work is going to be HTML, CSS, and JavaScript.
However, I could imagine Haskell replacing PHP for web services
and/or being used to write utilities and code generators.
