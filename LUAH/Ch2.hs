-- most functions are prefix.
-- ex: succ 9
-- 	   prev 3
-- 	   min 9 10
-- etc
--
-- however, we have infix functions too,
-- ex: 8 * 4
-- 	   9 + 3
-- etc

-- elem takes a thing and a list of things and tells us if that thing is an
-- element of the list. It's usually called as an infix function because it's
-- easier to read that way.

elem1 = 4 `elem` [3,4,5,6]
elem2 = 10 `elem` [30,30,20]


-- list comprehensions are cool
lc1 = [x*2 | x <- [1..10]]

-- add a predicate, x*2 >= 12
-- so this reads as "give me all the doubled x's from 1-10 where the double 
-- >= 12
lc2 = [x*2 | x <- [1..10], x*2 >= 12]
-- add another predicate
lc3 = [x*2 | x <- [1..10], x*2 >= 12, x*2 == 14]

-- Cool, it works. How about if we wanted all numbers from 50 to 100 whose
-- remainder when divided with the number 7 is 3? Easy.
lc4 = [x | x <- [50..100], x `mod` 7 == 3]

-- Weeding out lists by PREDICATES is called FILTERING
--
-- Success! Note that weeding out lists by predicates is also called filtering.
-- We took a list of numbers and we filtered them by the predicate. Now for
-- another example. Let's say we want a comprehension that replaces each odd
-- number greater than 10 with "BANG!" and each odd number that's less than 10
-- with "BOOM!". If a number isn't odd, we throw it out of our list. For
-- convenience, we'll put that comprehension inside a function so we can easily
-- reuse it.

-- here, odd is the predicate
boomBangs xs = [ if x < 10 then "Boom" else "BANG!" | x <- xs, odd x ]

-- splitting into smaller chunks and defining function type
boomBang x = if x < 10 then "Boom" else "BANG!"
newBoomBangs xs = [ boomBang x | x <- xs, odd x ]


-- list comprehension with multiple lists
-- We have two lists, [2,5,10] and [8,10,11] and we want to get the products
-- of all the possible combinations between numbers in those lists, here's what
-- we'd do.
lc5 = [ x*y | x <- [2,5,10], y <- [8,10,11]]  

-- making a function for funs
listProduct :: Num t => [t] -> [t] -> [t]
listProduct xs ys = [ x*y | x <- xs, y <- ys]

-- dumb length
-- _ means that we don't care what we'll draw from the list anyway so instead of
-- writing a variable name that we'll never use, we just write _. This function
-- replaces every element of a list with 1 and then sums that up. This means
-- that the resulting sum will be the length of our list.
length' xs = sum [1 | _ <- xs]  


-- Just a friendly reminder: because strings are lists, we can use list
-- comprehensions to process and produce strings. Here's a function that takes a
-- string and removes everything except uppercase letters from it.
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

