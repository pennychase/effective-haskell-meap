module BinaryTree where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
    deriving Show

showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch left str right) =
    showStringTree left <> str <> " " <> showStringTree right
 

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf n = Branch Leaf n Leaf
addElementToIntTree t@(Branch l n r) m =
    case compare m n of
        EQ -> t
        LT -> Branch (addElementToIntTree l m) n r
        GT -> Branch l n (addElementToIntTree r m)

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l n r) m =
    if n == m
        then True
        else doesIntExist l m || doesIntExist r m


