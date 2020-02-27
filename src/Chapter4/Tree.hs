module Chapter4.Tree where

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Leaf
  deriving (Show)

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind t (Node v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind t l
    GT -> treeFind t r
treeFind _ Leaf = Nothing

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert t n@(Node v l r) =
  case compare t v of
    EQ -> n
    LT -> Node v (treeInsert t l) r
    GT -> Node v l (treeInsert t r)
treeInsert t Leaf = Node t Leaf Leaf

treeConcat :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeConcat x y = foldl (flip treeInsert) x $ collect y
                 where collect (Node v l r) = v : collect l ++ collect r
                       collect Leaf = []

testTree :: BinaryTree Int
testTree = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 12 Leaf Leaf))

testTree1 :: BinaryTree Int
testTree1 = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) Leaf

testTree2 :: BinaryTree Int
testTree2 = Node 7 Leaf (Node 11 (Node 8 Leaf Leaf) (Node 12 Leaf Leaf))