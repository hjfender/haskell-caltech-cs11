module Lab3part2 (Quaternion
                 ) where

{- Quaternion Type -}
data Quaternion = Quaternion { r, i, j, k :: Double }
                deriving (Eq)

instance Show Quaternion where
show (Quaternion w x y z) = (Prelude.show w) ++ " + " ++ (Prelude.show x) ++ "i + " ++ (Prelude.show y) ++ "j + " ++ (Prelude.show z) ++ "k"

{- Assuming the axiom of choice, every set can be well-ordered, so we have to be somewhat specific.
The quaternions cannot be ordered in the following sense...
Suppose that we can order the quaternions so that we have positive and negative quaternions
and zero, such that the isomorphic copy of the real numbers in the quaternions split into their normal categories.
This split must satisfies several properties. In particular, no quaternion multiplied by
another can equal a negative quaternion. However, i*i = k*k = j*j = -1, contradict this stricture.

Thus the quaternions cannot be usefully ordered for an Ord implementation -}

instance Num Quaternion where
(+) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion ((Prelude.+) a1 a2) ((Prelude.+) b1 b2) ((Prelude.+) c1 c2) ((Prelude.+) d1 d2)
(-) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion ((Prelude.-) a1 a2) ((Prelude.-) b1 b2) ((Prelude.-) c1 c2) ((Prelude.-) d1 d2)
(*) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion (foldl (Prelude.-) ((Prelude.*) a1 a2) [((Prelude.*) b1 b2),((Prelude.*) c1 c2),((Prelude.*) d1 d2)])
                                                                   (foldl (Prelude.+) (negate((Prelude.*) d1 c2)) [((Prelude.*) c1 d2),((Prelude.*) a1 b2),((Prelude.*) b1 a2)])
                                                                   (foldl (Prelude.+) (negate((Prelude.*) b1 d2)) [((Prelude.*) d1 b2),((Prelude.*) a1 c2),((Prelude.*) c1 a2)])
                                                                   (foldl (Prelude.+) (negate((Prelude.*) c1 b2)) [((Prelude.*) b1 c2),((Prelude.*) a1 d2),((Prelude.*) d1 a2)])
abs (Quaternion a b c d) = Quaternion (sqrt(foldr (Prelude.+) 0.0 [((Prelude.*) a a),((Prelude.*) b b),((Prelude.*) c c),((Prelude.*) d d)])) 0 0 0
signum x@(Quaternion a b c d) = Quaternion (a / (r $ Lab3part2.abs x)) (b / (r $ Lab3part2.abs x)) (c / (r $ Lab3part2.abs x)) (d / (r $ Lab3part2.abs x))
fromInteger i = Quaternion (i :: Double) 0 0 0

testQuaternions :: IO ()
testQuaternions = do s1 <- return ("i*i: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 1.0 0.0 0.0) (Quaternion 0.0 1.0 0.0 0.0)))
                     putStrLn s1
                     s2 <- return ("j*j: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 1.0 0.0) (Quaternion 0.0 0.0 1.0 0.0)))
                     putStrLn s2
                     s3 <- return ("k*k: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 0.0 1.0) (Quaternion 0.0 0.0 0.0 1.0)))
                     putStrLn s3
                     s4 <- return ("i*j: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 1.0 0.0 0.0) (Quaternion 0.0 0.0 1.0 0.0)))
                     putStrLn s4
                     s5 <- return ("j*i: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 1.0 0.0) (Quaternion 0.0 1.0 0.0 0.0)))
                     putStrLn s5
                     s6 <- return ("j*k: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 1.0 0.0) (Quaternion 0.0 0.0 0.0 1.0)))
                     putStrLn s6
                     s7 <- return ("k*j: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 0.0 1.0) (Quaternion 0.0 0.0 1.0 0.0)))
                     putStrLn s7
                     s8 <- return ("k*i: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 0.0 0.0 1.0) (Quaternion 0.0 1.0 0.0 0.0)))
                     putStrLn s8
                     s9 <- return ("i*k: " ++ Lab3part2.show ((Lab3part2.*) (Quaternion 0.0 1.0 0.0 0.0) (Quaternion 0.0 0.0 0.0 1.0)))
                     putStrLn s9
