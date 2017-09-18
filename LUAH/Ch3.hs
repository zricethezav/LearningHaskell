-- Functions also have types. When writing our own functions, we can choose to
-- give them an explicit type declaration. This is generally considered to be
-- good practice except when writing very short functions. From here on, we'll
-- give all the functions that we make type declarations. Remember the list
-- comprehension we made previously that filters a string so that only caps
-- remain? Here's how it looks like with a type declaration.
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- some confusing shit
-- The parameters are separated with -> and there's no special distinction
-- between the parameters and the return type. The return type is the last item
-- in the declaration and the parameters are the first three. Later on we'll see
-- why they're all just separated with -> instead of having some more explicit
-- distinction between the return types and the parameters like Int, Int, Int ->
-- Int or something. 
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

-- if we hop into the ghci interpreter and hit it with `:t head` we will see
-- head :: [a] -> a
--
-- What is a? 
--
-- a is a type variable meaning it can be of any type, much like generics in
-- other languages.
--
-- Functions that have type variables are called POLYMORPHIC
--


-- TYPECLASSES
-- A typeclass is a sort of interface that defines some behavior.
--
--     ghci> :t (==)  
--     (==) :: (Eq a) => a -> a -> Bool  
--
-- Everything before the => symbol is called a class constraint. We can read the
-- previous type declaration like this: the equality function takes any two
-- values that are of the same type and returns a Bool
--
-- The Eq typeclass provides an interface for testing for equality. 
-- Ord is for types that have an ordering
-- Show can be presented as strings
-- Read is sort of the opposite typeclass of Show
-- Enum members are sequentially ordered types -- they can be enumerated.
-- Bounded members have an upper and a lower bound
-- Num is a numeric typeclass. Its members have the property of being able to
-- act like numbers. Let's examine the type of a number.
