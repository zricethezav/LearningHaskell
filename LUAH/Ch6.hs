-- Higher order functions 
--
-- Haskell functions can take funcs as params and returns funcs as return
-- values. A function that does either of those is called a higher order
-- functions. Pretty much this is Haskell. Higher functions allow you to define
-- what things ARE rather than defining steps that change some state.
--
-- Every function in Haskell only takes one parameter. All functions that accept
-- several parameters are called CURRIED FUNCTIONS.
--
-- These two are equivelent:
-- max 4 5   <--- syntax makes use of FUNCTION APPLICATION
-- (max 4) 5
--
-- Function application syntax is kinda like allowing the space to act as an
-- high precedent operator.
--
-- Max's function declaration could also be written as
-- max :: (Ord a) => a -> a -> a
-- max :: (Ord a) => a -> (a -> a) 
-- That could be read as: max takes an a and returns (that's the ->) a function
-- that takes an a and returns an a. That's why the return type and the
-- parameters of functions are all simply separated with arrows.


-- Partially applied functions... allow us to provide as little params as we
-- want.
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  
multTwoWithNine = multThree 9
finished = multTwoWithNine 2 3

-- By calling functions with too few parameters, so to speak, we're creating new
-- functions on the fly. What if we wanted to create a function that takes a
-- number and compares it to 100? We could do something like this:
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
-- If we call it with 99, it returns a GT. Simple stuff. Notice that the x is on
-- the right hand side on both sides of the equation. Now let's think about what
-- compare 100 returns. It returns a function that takes a number and compares
-- it with 100. Wow! Isn't that the function we wanted? We can rewrite this as:
compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100  
-- we can do this remember because compareWithHundred' is a partial function
-- from 'compare' so its waiting for the next arg


-- cool partial func example
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

-- Functions can take functions as parameters and also return functions. To
-- illustrate this, we're going to make a function that takes a function and
-- then applies it twice to something!
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  
