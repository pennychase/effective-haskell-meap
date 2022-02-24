module Peano where

data Peano = Z | S Peano
    deriving Show

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano (n - 1))

fromPeano :: Peano -> Int 
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
    case (p, p') of
        (Z, Z) -> True 
        (S n, S n') -> eqPeano n n'
        _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z p = p
addPeano (S n) p = addPeano n (S p)

mulPeano :: Peano -> Peano -> Peano
mulPeano Z _ = Z
mulPeano (S n) p = addPeano (mulPeano n p) p