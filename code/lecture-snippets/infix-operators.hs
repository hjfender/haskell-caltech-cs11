(++*) :: [a] -> [a] -> [a]
[] ++* ys = ys
(x:xs) ++* ys = x : (xs ++* ys)

(.*) :: (b->c) -> (a->b) -> (a->c)
f .* g = \x -> f (g x)

--declare fixity separately
infixr 5 ++* 

infixr 9 .*

--infixr: right-associative
--infixl: left-associative
--infix:  non-associative