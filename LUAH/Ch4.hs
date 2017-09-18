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
