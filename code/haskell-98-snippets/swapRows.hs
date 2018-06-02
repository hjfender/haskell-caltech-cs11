import Data.Array

-- Inefficient
swapRows' :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows' i i' a = a // ([((i,j), a!(i',j)) | j <- [jLo..jHi]] ++
                        [((i',j), a!(i,j)) | j <- [jLo..jHi]])
                  where ((iLo,jLo),(iHi,jHi)) = bounds a

swapRows :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows i i' a = a // [assoc | j <- [jLo..jHi],
                                assoc <- [((i,j), a!(i',j)),
                                          ((i',j),a!(i,j))] ]
                                where ((iLo,jLo),(iHi,jHi)) = bounds a