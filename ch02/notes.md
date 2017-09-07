## Why care about types?
In Haskell, every expression and function has a type. The type of a value
enables the sharing of properties between similar types. 

If describing expressions we would say, "this expression has type X" or "this
expression is of type X".

What exactly are 'Types'. Types are just an abstraction that describe what
certain bytes in your system are. Not all type systems are the same for
different languages.

## Haskell's Type System

### Strong, Static, Inferred
Strong type systems guarantee prevention of certain kinds of types errors your
may find in other languages. In Haskell, if you define a function that accepts
Strings and give it an Integer, the compiler will reject and throw an error.

Strong typing in Haskell does not allow casting like in C -- you can't pass a
byte array into a function and then cast those bytes into a complex
datastructure, a nice efficiency in C.

Statically typed systems means that the compiler knows the type of every value
and expression at compile time. (Python is a "duck typing" system where you can
treat similar objects as the same type.) <-- review this definition

Haskell can infer the type of most expressions and therefore its not necessary
to dclare the type of any value or expression. However, declaring types is the
convention and should be followed for more readable code.

### Lists and Tuples
List are polymorphic, meaning you can have a list of Bool types or a list of Int
types, etc. We want to use a polymorphic type, like a list, we use type variable
which requires a lower case letter as a type variable placeholder and will
eventually get subsituted by a real type. ex: `ghci> :type [1,2,3]`

### Functions and Lists
NOTE: Record this maybe?
Exercise to do: build the lines function, but the delimiter is a comma
```
:type lines
lines :: String -> [String]
```
This translates to "Lines has the type String to list of String"


## Side Effects
Side effects in a programming language indicate that the function has a
dependency on the global state of the program running. An example would be, if a
function branches based on the value of a modifyable global.

Haskell supports side effects in its language and does so by the type of the
function: `IO`... we'll learn more about this later, but all I know is `IO` is a
monad.











