-- this demonstrates the let binding
factorial :: Int -> Int
factorial n =
    let iter n r =
            if n == 0 then r
            else iter (n-1) (n*r)
    in iter n 1