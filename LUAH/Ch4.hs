-- Syntax in Functions
--
-- Pattern Matching
-- When defining functions, you can define SEPERATE FUNCTION BODIES for
-- different patterns.
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven"
lucky x = "sorry"

-- When lucky is called, the patterns will be checked from top to bottom. When
-- the function confirms a pattern, the function body of the match will be used. 

-- Lets define a factorial function with pattern matching:
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- When defining patterns we should always have a catchall in case the input
-- does not match any patterns provided. Example:
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bob"
-- What happens when we call charName with 'h'... an exception for
-- "Non-exhaustive patterns"


-- lets have a pattern include the '_' variable
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x


-- Lists themselves can also be patterned matched.
-- The most common pattern is x:xs which is just saying that the function body
-- will bind the head of a list to x and the rest of the list to xs
-- 
-- Doing more complicated things with list pattern matching is possible too
-- x:y:z:zs will bind the first three elements of a list to variables and the
-- tail of the list to another variable, zs.
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

-- sumFirstThree :: [a] -> a
sumFirstThree :: Num a => [a] -> a
sumFirstThree ([]) = error "Need three elements!!" 
sumFirstThree (_:[]) = error "Need three elements!!" 
sumFirstThree (_:_:[]) = error "Need three elements!!" 
sumFirstThree (x:y:z:_) = x + y + z 

-- more shit
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  


-- length using recursion and pattern matching
-- remember (_:xs) is syntactic sugar for ignoring the first element of the list
-- and assigning the tail of the list to xs
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

-- There's also a thing called AS PATTERNS. Those are a handy way of breaking
-- something up according to a pattern and binding it to names whilst still
-- keeping a reference to the whole thing. You do that by putting a name and an
-- @ in front of a pattern. For instance, the pattern xs@(x:y:ys). This pattern
-- will match exactly the same thing as x:y:ys but you can easily get the whole
-- list via xs instead of repeating yourself by typing out x:y:ys in the
-- function body again. Here's a quick and dirty example:
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

-- Guards are indicated by pipes that follow a function's name and its
-- parameters. Usually, they're indented a bit to the right and lined up. A
-- guard is basically a boolean expression. If it evaluates to True, then the
-- corresponding function body is used. If it evaluates to False, checking drops
-- through to the next guard and so on. If we call this function with 24.3, it
-- will first check if that's smaller than or equal to 18.5. Because it isn't,
-- it falls through to the next guard. The check is carried out with the second
-- guard and because 24.3 is less than 25.0, the second string is returned.
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

isFirstA :: String -> Bool
isFirstA xs
    | head xs == 'a' = True
    | otherwise = False


-- WHERE?
-- where comes after the guards and can define variables
bmiTellNew :: (RealFloat a) => a -> a -> String
bmiTellNew weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
        

-- where again
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
-- where bindings can also be nested. It's a common idiom to make a function and
-- define some helper function in its where clause and then to give those
-- functions helper functions as well, each with its own where clause.


-- Let
cylinder :: (RealFloat a) => a -> a-> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea
-- As you can see, we could have also defined this with a where binding. Notice
-- that the names are also aligned in a single column. So what's the difference
-- between the two? For now it just seems that let puts the bindings first and
-- the expression that uses them later whereas where is the other way around.


-- let in list comprehensions
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  
-- We omitted the in part of the let binding when we used them in list
-- comprehensions because the visibility of the names is already predefined
-- there. However, we could use a let in binding in a predicate and the names
-- defined would only be visible to that predicate. The in part can also be
-- omitted when defining functions and constants directly in GHCi. If we do
-- that, then the names will be visible throughout the entire interactive
-- session.




-- Cases
-- Lets translate head' into a function that makes use of case expressions
oldHead' :: [a] -> a  
oldHead' [] = error "No head for empty lists!"  
oldHead' (x:_) = x  

-- translated
headCase :: [a] -> a  
headCase xs = case xs of [] -> error "No head for empty lists!"
                         (x:_) -> x

-- Case expressions have the following syntax 
--    case expression of pattern -> result  
--                       pattern -> result  
--                       pattern -> result  
--                       ...  


-- Whereas pattern matching on function parameters can only be done when
-- defining functions, case expressions can be used pretty much anywhere. For
-- instance:
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 


-- They are useful for pattern matching against something in the middle of an
-- expression. Because pattern matching in function definitions is syntactic
-- sugar for case expressions, we could have also defined this like so:
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  



