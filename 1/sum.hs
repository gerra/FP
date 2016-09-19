import Text.Regex (splitRegex, mkRegex)

stringSum :: String -> Int
--stringSum s = sum $ map read $ filter (\s -> (length s) > 0) $ splitRegex (mkRegex "(\\s)+") s
--stringSum = sum $ map read $ words
stringSum s = sum $ map read $ map deletePlus (words s)

deletePlus :: String -> String
deletePlus(c:s) = if (c == '+') then s else c:s