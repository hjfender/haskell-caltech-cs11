import Data.List
--
-- lab2.hs
--

-- Sum a list of integers recursively.
sumIntegers1 :: [Integer] -> Integer
sumIntegers1 (x:xs) = x + (sumIntegers1 xs)
sumIntegers1 [] = 0

-- Sum a list of integers in terms of foldl.
sumIntegers2 :: [Integer] -> Integer
sumIntegers2 = foldl (+) 0

-- Product of a list of integers in terms of foldr.
prodIntegers :: [Integer] -> Integer
prodIntegers = foldr (*) 1

-- List append in terms of foldr.
listAppend :: [a] -> [a] -> [a]
listAppend l1 l2 = foldr (:) l2 l1

-- Insertion sort.
-- This sorts a list of values (which all have to be of type class Ord)
-- according to the following algorithm:
-- 1) Sort the sublist consisting of the tail of the list.
-- 2) Insert the first element (the head) into the sorted sublist at the
--    correct place.
-- Sort the list in ascending order.
insertionSort :: Ord a => [a] -> [a]
insertionSort (x:xs) = insert x (insertionSort xs)
insertionSort [] = []

-- 'map' that works on two lists.
-- map f a b = [(f a1 b1), (f a2 b2), ...]
--     where a = [a1, a2, ...] and b = [b1, b2, ...]
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (a:as) (b:bs) = (f a b):(map2 f as bs)
map2 f [] [] = []

-- Infinite list of factorials, starting from 0!, using map2.
factorials :: [Integer]
factorials = map2 (*) (1:factorials) (1:[1..])

-- Infinite list of prime numbers using list comprehensions and a sieving
-- algorithm. The sieve works as follows: it's n followed by (the sieve of) 
-- all numbers that aren't divisible by n.
primes :: [Integer]
primes = [p | p <- sieve 2 [2..]]

sieve :: Integer -> [Integer] -> [Integer]
sieve n l = n:(sieve (head s) (tail s)) where
    s = filter (\x -> x `mod` n /= 0) l

-- Tree data structure.  Note that this is different from the Tree
-- structure used in the lecture.
data Tree a = Leaf | Node a (Tree a) (Tree a)
              deriving Show

-- Sample tree (for testing).
sampleTree :: Tree Integer
sampleTree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- Count the number of leaves in a Tree.
countLeaves :: Tree a -> Integer
countLeaves (Node _ t1 t2) = (countLeaves t1) + (countLeaves t2)
countLeaves Leaf = 1

-- Mapping over a tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)
mapTree f Leaf = Leaf

-- Some data/functions for testing:

-- Counting characters in a String.
countChars :: String -> Integer
countChars = toInteger . length

-- Sample tree of strings.
treeOfStrings :: Tree String
treeOfStrings = Node "foo" 
                (Node "bar" (Node "baz" Leaf Leaf) Leaf) 
                (Node "bam" Leaf Leaf)

treeOfNums :: Tree Integer
treeOfNums = mapTree countChars treeOfStrings


-- Fold on a tree.
foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree f x (Node y t1 t2) = f y (f (foldTree f x t1) (foldTree f x t2))
foldTree f x _ = x

-- Sample use of foldTree: count the characters in a tree of Strings.
charsInTree :: Integer
charsInTree = foldTree (+) 0 (mapTree countChars treeOfStrings)


--
-- end of lab2.hs
--
