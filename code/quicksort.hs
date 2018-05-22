quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort lt ++ [x] ++ quicksort ge
    where
        lt = [y | y <- xs, y < x]
        ge = [y | y <- xs, y >= x]