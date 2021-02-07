qsort2 :: Ord a => [a] -> [a] -- quick sort a list in Haskell
qsort2 [] = []  
qsort2 (x:xs) =   
    let smallerSorted = qsort2 [a | a <- xs, a <= x] -- List comprehension of values smaller than pivot
        biggerSorted = qsort2 [a | a <- xs, a > x]  -- List comprehension of values larger than pivot
    in  smallerSorted ++ [x] ++ biggerSorted -- Sort lists in respective sides of pivot
