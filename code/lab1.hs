add_and_double :: Double -> Double -> Double
add_and_double x y = 2 * (x + y)

(+*) :: Double -> Double -> Double
x +* y = x `add_and_double` y

solve_quadratic_equation :: Double -> Double -> Double -> (Double, Double)
solve_quadratic_equation a b c = let x = (-b + sqrt (b ** 2 - 4 * a * c)) / (2 * a)
                                     y = (-b - sqrt (b ** 2 - 4 * a * c)) / (2 * a)
                                 in (x, y)

first_n :: Int -> [Int]
first_n n = take n [1..]

first_n_integers :: Integer -> [Integer]
first_n_integers n = take_integer n [1..] where
                     take_integer 0 l = []
                     take_integer n l = if (n>0)
                                        then (head l):(take_integer (n-1) (tail l))
                                        else error "n is not positive"

double_factorial :: Integer -> Integer
double_factorial 0 = 1
double_factorial n = let
                        factorial 0 = 1
                        factorial n = n * (factorial (n-1))
                    in
                    if (n >= 0)
                    then (factorial n) * (double_factorial (n-1))
                    else error "n is negative"

-- This is fucking wild!
factorials :: [Integer]
factorials = (zipWith (*) (1:factorials) (1:[1..]))