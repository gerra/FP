import Data.List

zipN :: ([a] -> b) -> [[a]] -> [b]
--zipN f x = map f (transpose x)
zipN f = map f . transpose