--this demonstrates pattern matching
factorial :: Int -> Int
factorial n = iter n 1
    where
        iter 0 r = r
        iter n r = iter (n-1) (n*r)