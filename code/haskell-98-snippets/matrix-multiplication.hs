import Data.Matrix

matMult :: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y = array resultBounds
                    [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                    | i <- range (li,ui),
                                      j <- range (lj',uj')]
                    where ((li,lj),(ui,uj)) = bounds x
                          ((li',lj'),(ui',uj')) = bounds y
                          resultBounds
                            | (lj,uj) == (li',ui') = ((li,lj'),(uj,uj'))
                            | otherwise = error "matMult: incompatible bounds"

matMult' :: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult' x y = accumArray (+) 0 resultBounds
                          [((i,j), x!(i,k) * y!(k,j))
                                    | i <- range (li,ui),
                                      j <- range (lj',uj')
                                      k <- range (lj,uj)]
                where ((li,lj),(ui,uj)) = bounds x
                      ((li',lj'),(ui',uj')) = bounds y
                      resultBounds
                        | (lj,uj) == (li',ui') = ((li,lj'),(uj,uj'))
                        | otherwise = error "matMult: incompatible bounds"

genMatMult :: (Ix a, Ix b, Ix c) => ([f] -> g) -> (d -> e -> f) -> Array (a,b) d -> Array (b,c) e -> Array (a,c) g
genMatMult sum' star x y = array resultBounds
                                    [((i,j), sum' [x!(i,k) `star` y!(k,j) | k <- range (lj,uj)])
                                                                | i <- range (li,ui),
                                                                  j <- range (lj',uj')]
                                where ((li,lj),(ui,uj)) = bounds x
                                      ((li',lj'),(ui',uj')) = bounds y
                                      resultBounds
                                        | (lj,uj) == (li',ui') = ((li,lj'),(ui,uj'))
                                        | otherwise = error "genMatMult: incompatible bounds"