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

That actions and functions are separate but that functions can return
actions and actions can use functions was the biggest conceptual hurdle
I had to overcome when learning the language.  It doesn't help that
tutorials on the subject tend to make things a lot more complicated
than they have to by attempting to describe ```IO ()``` as something
to do with monads, which have something to do with category theory.
You don't need to know any of that stuff to write Haskell programs.

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

Where ```(\l -> ...)``` defines an anonymous function that takes 'l' and passes it to putStrLn.
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

Given that ```++``` is the string (also list) concatenation operator,
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

As a somewhat contrived example, let's say we want to extend our greet function
so that it adds an exclamation point on the end.  One way to do it would be to
use a named argument like this:

```haskell
greet name = "Hello, " ++ name ++ "!"
```

Or equivalently:

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

Here's another operator that I had a hard time with:

```haskell
=>
```

This operator is used in type expressions and means 'the type on the right, given the constraints on the left'.

I can't really give any good examples without explaining the type system, first.

#### Simple data structures

Haskell makes it really easy to define struct-like things with the ```data``` statement:

```haskell
data Person = Person { personName :: String, personFavoriteInteger :: Integer }
```

This is shorthand for

```haskell
data Person = Person String Integer
```

and then definig ```personName``` and ```personFavoriteInteger``` functions separately.

We can also define union types pretty easily:

```haskell
data AirOrWater = Air | Water
```

This declares that something of type ```AirOrWater``` is either an ```Air``` or a ```Water```.

We can define structures that contain values of arbitrary other types:

```haskell
data TreeNode t = Leaf t | Branch (TreeNode t) (TreeNode t)
```

Now when we declare a value to be of a ```TreeNode```, we need to give the type an argument, such as ```String```:

```haskell
coolTree :: TreeNode String
coolTree = Branch (Leaf "hi") (Branch (Leaf "bye") (Leaf "wat"))
```

#### Pattern matching

There area couple ways to define a function that operates differently
depending on what the input is:

- declare the acceptable values right in the signature
- use guards

```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

Guards are not quite as concise but are more flexible because you can
do checks that aren't available from the pattern-matching syntax:

```haskell
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)
```

#### Type classes

A level of abstraction above types are 'type classes'.

A 'type class' is like an interface in Java-like languages
(C#, PHP, etc).  One important type class is ```Show```, which is implemented by anything that
can be turned into a string.  To do this we write an ```instance``` statement.  Here we'll
define how the ```show``` function works (to implement the ```Show``` class) for our AirOrWater
type:

```haskell
instance Show AirOrWater where
  show Air = "air"
  show Water = "water"
```

Using ```instance``` and the ```=>``` operator we can implement ```Show``` for
instance of our Tree type that contain Show-able data.  Here's the complete program:

```haskell
-- Define our TreeNode type:
data TreeNode t = Leaf t | Branch (TreeNode t) (TreeNode t)

-- Show should be implemented for all TreeNode t where
-- t itself implements Show:
instance (Show t) => Show (TreeNode t) where
  show (Leaf s) = show s
  show (Branch l1 l2) = "(" ++ (show l1) ++ " " ++ (show l2) ++ ")"

-- Define a tree of Strings
coolTree :: TreeNode String
coolTree = Branch (Leaf "hi") (Branch (Leaf "bye") (Leaf "wat"))

-- Print them
main :: IO ()
main = putStrLn (show coolTree)
```

#### Aliasing types

You can alias types by using ```type``` statements:

```haskell
type StringToStringConverter = String -> String

appendStuff :: StringToStringConverter
appendStuff = (++ " and stuff")

main :: IO ()
main = putStrLn $ appendStuff "Mockey Mouse"
```

#### Lazy evaluation

Due to its purely functional nature, expressions in Haskell are
'referentially transparent'.  This means that you or the compiler
may freely replace function calls with the result of the function


## GHCI

The first hurdle I had to overcome when learning Haskell was that
you have to define things slightly differently in ```ghci``` than
in your actual source code.  You can't just copy-paste between them
like you can in many other languages with interactive interpreters.

For instance, to define something with a name, you need to say ```let```,
as in:

```haskell
let greeting = "Hello there, "
let greet name = greeting ++ name ++ "!"
greet "Nancy"
```

It's difficult to play around with types in the interactive interpreter
because you can't declare things before they're defined.  You can sort
of do it by including types in your definitions and making sure
they still compile:

```haskell
let addTen = (+ 10) :: Integer -> Integer
```

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
