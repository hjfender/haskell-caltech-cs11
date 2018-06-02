import Data.Array

wavefront :: Int -> Array (Int,Int) Int
wavefront n = a where
              a = array ((1,1),(n,n))
                    ([((1,j),1) | j <- [1..n]] ++
                     [((i,1),1) | i <- [2..n]] ++
                     [((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                | i <- [2..n], j <- [2..n]])