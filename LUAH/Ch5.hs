-- This section is an overview of recursion. Quicksort is the posterchild of
-- Haskell so lets implement it...
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- OR using where instead of let
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = quicksort' [a | a <- xs, a <= x]
          biggerSorted = quicksort' [a | a <- xs, a > x]
