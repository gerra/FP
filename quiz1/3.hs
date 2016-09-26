data StackWithMin a = Empty | Top (a, a) (StackWithMin a) deriving Show

push :: (Ord a) => StackWithMin a -> a -> StackWithMin a
push Empty a = Top (a,a) Empty
push (Top (e, m) s) a = Top (a, if a < m then a else m) (Top (e, m) s)

pop :: StackWithMin a -> StackWithMin a
pop Empty = Empty
pop (Top _ s) = s

multipop :: StackWithMin a -> Int -> StackWithMin a
multipop s x = if x <= 0 then s else multipop (pop s) (x - 1)

getMin :: StackWithMin a -> a
getMin Empty = error("empty_stack")
getMin (Top (_, m) _) = m 

