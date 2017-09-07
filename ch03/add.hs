-- file: ch03/add.hs
add a b = a + b

-- bleep
-- Let us step through the evaluation of sumList [1,2]. The list notation [1,2]
-- is shorthand for the expression (1:(2:[])). We begin by trying to match the
-- pattern in the first equation of the definition of sumList. In the (x:xs)
-- pattern, the “:” is the familiar list constructor, (:). We are now using it
-- to match against a value, not to construct one. The value (1:(2:[])) was
-- constructed with (:), so the constructor in the value matches the constructor
-- in the pattern. We say that the pattern matches, or that the match succeeds
sumList (x:xs) = x + sumList xs
sumList [] = 0

complicated (True, a, x:xs, 5) = (a, xs)
complicated (False, a, x:xs, 5) = (a, reverse xs)
