data BTree a = Node {element :: a, left :: BTree a , right :: BTree a } | Empty deriving (Show, Read, Eq, Ord)

initBTree :: BTree a
initBTree = Empty

addBTree :: (Ord a) => BTree a -> a -> BTree a
addBTree Empty elem = Node elem Empty Empty
addBTree (Node e l r) elem
  | elem == e = Node elem l r
  | elem < e =  Node e (addBTree l elem) r
  | elem > e = Node e l (addBTree r elem)