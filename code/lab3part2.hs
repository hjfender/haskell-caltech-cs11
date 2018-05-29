module Lab3part2 (Quaternion
                 ) where

{- Quaternion Type -}
data Quaternion = Quaternion Double Double Double Double
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

