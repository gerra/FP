import System.Random (newStdGen, randomRs)
import Control.Applicative

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

sort :: [Int] -> [Int]
sort a = if length a < 2
	then a
	else merge left right where
		left = sort (take m a)
		right = sort (drop m a)
		m = length a `div` 2

merge :: [Int] -> [Int] -> [Int]
merge [] a = a
merge a [] = a
merge (a:as) (b:bs) = if a < b 	
	then a:(merge as (b:bs)) 
	else b:(merge (a:as) bs)