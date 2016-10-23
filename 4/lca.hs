data InNode a = Node { label :: a, parent :: Maybe (InNode a) } deriving Show
 
n1 = Node {label = 1, parent = Nothing}
n2 = Node {label = 2, parent = Just n1}
n3 = Node {label = 3, parent = Just n1}
n4 = Node {label = 4, parent = Nothing}

checkFirstIsParent :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> Bool
checkFirstIsParent Nothing _ = False
checkFirstIsParent _ Nothing = False
checkFirstIsParent (Just n1) (Just n2) = if (label n1) == (label n2)
    then True
    else checkFirstIsParent (Just n1) (parent n2)

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor n1 n2 = if checkFirstIsParent (Just n1) (Just n2)
    then Just n1
    else if checkFirstIsParent (Just n2) (Just n1)
        then Just n2 
        else safeLeastCommonAncestor (parent n1) (parent n2)

safeLeastCommonAncestor :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> Maybe (InNode a)
safeLeastCommonAncestor Nothing _ = Nothing
safeLeastCommonAncestor _ Nothing = Nothing
safeLeastCommonAncestor (Just n1) (Just n2) = leastCommonAncestor n1 n2