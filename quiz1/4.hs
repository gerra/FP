data Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
	show (Matrix []) = ""
	show (Matrix [x:xs]) = show x ++ "\n" ++ show (Matrix [xs])
	show [] = ""
	show (a:as) = a ++ " " + (show as) 