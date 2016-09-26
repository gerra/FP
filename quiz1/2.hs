primesCount :: [Int] -> Int
primesCount [] = 0
primesCount (x:xs) = (primesCount xs) + if isPrime x then 1 else 0

isPrime :: Int -> Bool
isPrime n
  | n <= 2 = n == 2
  | otherwise = odd n && divide 3
  where
    divide factor
      | factor * factor > n = True
      | otherwise = n `rem` factor /= 0 && divide (factor + 2) 