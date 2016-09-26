import Data.List

get :: String -> [String]
get s = [ x | x <- (inits s) , y <- (tails s), x == y]