data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x left right) minVal maxVal = 
  let 
    leftSorted  = isSortedTree left minVal x
    rightSorted = isSortedTree right x maxVal
  in
    x >= minVal && x <= maxVal && leftSorted && rightSorted
    

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x left Leaf) = Node x left (Node (x+1) Leaf Leaf)
addNewMax (Node x left right) = Node x left (addNewMax right)


insertTreeVal :: Tree -> Int -> Tree
insertTreeVal Leaf x = Node x Leaf Leaf
insertTreeVal (Node o left right) x
  | x == o    = Node o left right
  | x > o     = Node o left (insertTreeVal right x)
  | otherwise = Node o (insertTreeVal left x) right
  

treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node x left right) = 
  treeToList left ++ [x] ++ treeToList right