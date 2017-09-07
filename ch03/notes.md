### Conditionals
Haskell is an expression-oriented language and thus requires an `else` following
and `if` keyboard. The branches need to be complete so the program can fall
through nicely through the expressions.

`null {some variable}` is how we check for empty lists

We will usually break an if expression across several lines to keep the predicate
and each of the branches easier to follow

### Understanding evaluation by example

## Lazy Evaluation
Function to investigate: `isOdd n = mod n 2 == 1`

Here, mod is the standard modulo function. The first big step to understanding
how evaluation works in Haskell is figuring out what the result of evaluating
the expression isOdd (1 + 2) is.

Before we explain how evaluation proceeds in Haskell, let us recap the sort of
evaluation strategy used by more familiar languages. First, evaluate the
subexpression 1 + 2, to give 3. Then apply the odd function with n bound to 3.
Finally, evaluate mod 3 2 to give 1, and 1 == 1 to give True.

In a language that uses strict evaluation, the arguments to a function are
evaluated before the function is applied. Haskell chooses another path:
non-strict evaluation. 

In Haskell, the subexpression 1 + 2 is not reduced to the value 3. Instead, we
create a “promise” that when the value of the expression isOdd (1 + 2) is
needed, we'll be able to compute it. The record that we use to track an
unevaluated expression is referred to as a thunk. This is all that happens: we
create a thunk, and defer the actual evaluation until it's really needed. If the
result of this expression is never subsequently used, we will not compute its
value at all. 

Non-strict evaluation is often referred to as lazy evaluation

Laziness leads us to defer evaluation until we need a value, and to evaluate
just enough of an expression to establish its value.

The result of applying a function may be a thunk (a deferred expression).
