module BinaryTree where

data BinaryTree a
  = Empty
  | Leaf a
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Leaf a) = [a]
preOrder (Node left a right) = [a] ++ preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Leaf a) = [a]
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert a Empty = Node Empty a Empty
insert a (Leaf b)
  | a == b = Leaf a
  | a < b = Node (Leaf a) b Empty
  | otherwise  = Node Empty b (Leaf a)
insert a (Node left b right)
  | a == b = Node left b right
  | a > b = Node left b (insert a right)
  | otherwise = Node (insert a left) b right

bt = Node (Node (Leaf 1) 4 (Node Empty 6 (Leaf 7))) 8 (Node Empty 10 (Leaf 14))
