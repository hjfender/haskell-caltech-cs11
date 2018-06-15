module Lab3part1 (AbstractInteger,
             normalize,
             ai_toInteger
             ) where

import qualified Prelude as P

{- Abstract Integer data type -}
data AbstractInteger = Zero
                    | Succ AbstractInteger
                    | Pred AbstractInteger
                    deriving (P.Eq, P.Show)

-- normalize method to ensure abstract integer is entirely constructed
-- of preds or succs or is zero
normalize :: AbstractInteger -> AbstractInteger
normalize (Succ x) = if isPred a
                     then strip a
                     else Succ a
                     where a = normalize x
normalize (Pred x) = if isSucc a
                     then strip a
                     else Pred a
                     where a = normalize x
normalize Zero = Zero

isPred :: AbstractInteger -> P.Bool
isPred (Pred _) = P.True
isPred _ = P.False

isSucc :: AbstractInteger -> P.Bool
isSucc (Succ _) = P.True
isSucc _ = P.False

strip :: AbstractInteger -> AbstractInteger
strip (Pred x) = x
strip (Succ x) = x
strip Zero = Zero

-- helper method for compare
-- works assuming the abstract integers have been normalized             
c :: AbstractInteger -> AbstractInteger -> P.Ordering
c (Succ x) (Succ y) = c x y
c (Pred x) (Pred y) = c x y
c Zero Zero = P.EQ
c _ (Succ _) = P.LT
c _ (Pred _) = P.GT
c (Succ _) _ = P.GT
c (Pred _) _ = P.LT

instance P.Ord AbstractInteger where
compare x y = c (normalize x) (normalize y)

(+.) :: AbstractInteger -> AbstractInteger -> AbstractInteger
x +. Zero = x
x +. (Succ y) = (Succ x) +. y
x +. (Pred y) = (Pred x) +. y

(-.) :: AbstractInteger -> AbstractInteger -> AbstractInteger
x -. Zero = x
x -. (Succ y) = (Pred x) -. y
x -. (Pred y) = (Succ x) -. y

(*.) :: AbstractInteger -> AbstractInteger -> AbstractInteger
(*.) _ Zero = Zero
(*.) Zero _ = Zero
(*.) x (Succ y) = (x *. y) + x
(*.) x (Pred y) = (x *. y) - x

abs' :: AbstractInteger -> AbstractInteger
abs' y@(Succ _) = y
abs' (Pred x) = Succ (abs' x)
abs' Zero = Zero

signum' :: AbstractInteger -> AbstractInteger
signum' (Succ _) = Succ Zero
signum' (Pred _) = Pred Zero
signum' Zero = Zero

instance P.Num AbstractInteger where
(+) x y = normalize ((normalize x) +. (normalize y))
(-) x y = normalize ((normalize x) -. (normalize y))
(*) x y = normalize ((normalize x) *. (normalize y))
abs x = abs' (normalize x)
signum x = signum' (normalize x)
fromInteger :: P.Integer -> AbstractInteger
fromInteger 0 = Zero
fromInteger x = if (P.<) x 0
                then Pred (fromInteger ((P.+) x 1))
                else Succ (fromInteger ((P.-) x 1))

ai_toInteger :: AbstractInteger -> P.Integer
ai_toInteger (Succ x) = (P.+) (ai_toInteger x) 1
ai_toInteger (Pred x) = (P.-) (ai_toInteger x) 1
ai_toInteger Zero = 0