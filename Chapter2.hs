module Chapter2 where

-- Reversing

myReverse1 :: [a] -> [a]
myReverse1 = foldl (flip (:)) []

myReverse2 :: [a] -> [a]
myReverse2 = foldr (\x xs -> xs ++ [x]) []

-- zipping lists

myZipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith1 f xs ys = [ f x y | (x, y) <- zip xs ys]

myZipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith2 f xs ys = foldr (\p zs -> (uncurry f p) : zs) [] (zip xs ys)

myZipWith3 :: (a -> b -> c) -> [a]-> [b] -> [c]
myZipWith3 f [] _ = []
myZipWith3 f _ [] = []
myZipWith3 f (x:xs) (y:ys) = f x y : myZipWith3 f xs ys

-- Implementing concatMap

myConcatMap1 :: Foldable t => (a -> [b]) -> t a -> [b]
myConcatMap1 f = foldr ((++) . f) [] 

myConcatMap2 :: Foldable t => (a -> [b]) -> t a -> [b]
myConcatMap2 f = reverse . foldl (\acc x -> f x ++ acc) []