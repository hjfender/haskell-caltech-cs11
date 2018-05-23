--this demonstrates the where binding
factorial :: Int -> Int
factorial n = iter n 1
    where iter n r =
            if n == 0 then r
            else iter (n-1) (n*r)