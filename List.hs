module List where

-- Chapter 4: Inductively Defined List type

data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

toList' :: [a] -> List a
toList' = foldr Cons Empty

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

fromList' :: List a -> [a]
fromList' = listFoldr(:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr f x Empty = x
listFoldr f x (Cons y ys) = f y (listFoldr f x ys)

listFoldl ::(b -> a -> b) -> b -> List a -> b
listFoldl f x Empty = x
listFoldl f x (Cons y ys) = listFoldl f (f x y) ys

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ xs) = xs

listReverse :: List a -> List a
listReverse = listFoldl (flip Cons) Empty

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)