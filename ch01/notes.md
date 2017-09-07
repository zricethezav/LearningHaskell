## Getting Started


Use the Glassgow Haskell Compiler (GHC). GHC comes loaded with a compiler `ghc` and
interpretter `ghci`. The interpretter, ghci, is much like Python's `python`
interpretter. 

When you fire up `ghci` youl see a message like this,

```
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude>
```

What does that "Prelude" represent in the prompt? Prelude is a standard
library the interpretter comes loaded with. If we import additional libraries
you will see your prompt grow:

```
Prelude> import Data.List
Prelude Data.List>
```

The default behavior of the prompt can be changed like this:
```
Prelude Data.List> :set prompt "ghci>"
ghci>
```


## Math Notations

Write math operations like equations you did in school, pay close attention to
the parens otherwise you may run into:
```
ghci>-3 + 3
0
ghci>-3 + 3 + -3

<interactive>:6:1: error:
    Precedence parsing error
            cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same
            infix expression
```

With proper parens:
```
ghci>-3 + 3 + (-3)
-3
ghci>
```
Being generous on parens isn't a bad thing as it will help reduce parsing
errors.

```
ghci> 3*-3

<interactive>:11:2: error:
    • Variable not in scope: (*-) :: Integer -> Integer -> t
        • Perhaps you meant one of these:
                ‘*’ (imported from Prelude), ‘-’ (imported from Prelude),
                        ‘*>’ (imported from Prelude)
```
Here we see that `*-` is not defined. `*-` is interpreted as an operated.


``` 
:set +t 
``` for additional information output in the interpretter







