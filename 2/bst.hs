import TreePrinters

--data Tree a = Leaf | Node a (Tree a) (Tree a)

find :: (Ord a) => Tree a -> a -> Bool
find Leaf _ = False
find (Node a l r) x
	| x == a = True
	| x  < a = find l x
	| x  > a = find r x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node a l r) x
	| x == a = Node a l r
	| x  < a = Node a (insert l x) r
	| x  > a = Node a l (insert r x)

delete :: (Ord a) => Tree a -> a -> Tree a
delete Leaf x = Leaf
delete (Node a l r) x
	| x == a = deleteRoot (Node a l r)
	| x  < a = Node a (delete l x) r
	| x  > a = Node a l (delete r x)

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot (Node a Leaf r) = r
deleteRoot (Node a l Leaf) = l
deleteRoot (Node a l r)    = Node leftest l (delete r leftest) where leftest = (findLeftest r)

findLeftest :: (Ord a) => Tree a -> a
findLeftest (Node a Leaf _) = a
findLeftest (Node _ l _)    = findLeftest l

toList :: (Ord a) => Tree a -> [a]
toList Leaf = []
toList (Node a l r) = (toList l) ++ [a] ++ (toList r)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert (fromList xs) x