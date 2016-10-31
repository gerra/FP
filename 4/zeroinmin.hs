manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroInMin :: (Int, Int) -> Int
zeroInMin (a, b) = f [(a, b)]
    where
        f :: [(Int, Int)] -> Int
        f h
            | any (\(a, b) -> a == 0 && b == 0) h = 0
            | otherwise = f (h >>= manHeaps) + 1


date State s = State {g :: s -> (v, s)}
m a -> (a -> m b) -> m b
(State s) v
   a >>= f = State {g = \s -> (fst $ f a s)}

