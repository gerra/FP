tail' :: [a] -> [a]
tail' [] = []
tail' [x] = []
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [] = []
init' [a] = []
init' (x:xs) = x:(init' xs)

convert :: Maybe [a] -> [a]
convert (Just a) = a
convert Nothing = []

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
--safeTail [x] = Nothing
safeTail x = Just (tail' x)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
--safeInit [x] = Nothing
safeInit x = Just (init' x)

strip :: [a] -> [a]
strip = convert . safeTail . convert . safeInit