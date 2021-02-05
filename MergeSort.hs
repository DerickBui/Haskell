-- Derick Bui
-- Function that merges two lists into sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs -- if right list is empty
merge [] ys = ys -- if left list is empty
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

msort :: Ord a => [a] -> [a] -- merge sort a list in Haskell
msort [] = []
msort [a] = [a]
-- msort recursively merges the msort of two halves
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))
  -- create a list that merges the two halves
  where firstHalf  xs = take (length xs `div` 2) xs -- create left half of the list by taking the left half (takes first half of the list)
        secondHalf xs = drop (length xs `div` 2) xs -- create right half of the list by dropping the left half (drops first half of the list)
