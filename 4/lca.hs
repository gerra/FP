data InNode a = Node { label :: a, parent :: Maybe (InNode a) } deriving Show
 
n1 = Node {label = 1, parent = Nothing}
n2 = Node {label = 2, parent = Just n1}
n3 = Node {label = 3, parent = Just n1}
n4 = Node {label = 4, parent = Nothing}

instance Eq a => Eq (InNode a) where
    f == s = (label f) == (label s)

checkFirstIsParent :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> Bool
checkFirstIsParent Nothing _ = False
checkFirstIsParent _ Nothing = False
checkFirstIsParent n1 n2
    | n1 == n2  = True
    | otherwise = checkFirstIsParent n1 (n2 >>= parent)

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor n1 n2 
    | checkFirstIsParent f s = f
    | checkFirstIsParent s f = s
    | otherwise = safeLeastCommonAncestor f s
    where 
        f = return n1
        s = return n2

safeLeastCommonAncestor :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> Maybe (InNode a)
safeLeastCommonAncestor n1 n2 
    | n1 == n2  = n1
    | otherwise = safeLeastCommonAncestor (n1 >>= parent) (n2 >>= parent)