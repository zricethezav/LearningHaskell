-- file ch03/BadPattern.hs
badExample (x:xs) = x + badExample xs
-- Whats the bug??
-- ghci > badExample [] 
--
-- ^^ no definition for that pattern!

-- We could do:
-- badExample [] = 0
--
-- But  we can use a more general approach with wildcards:

goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
