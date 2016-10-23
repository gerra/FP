import Text.Regex (splitRegex, mkRegex)
import Data.Char

stringSum :: String -> Int
--stringSum s = sum $ map read $ filter (\s -> (length s) > 0) $ splitRegex (mkRegex "(\\s)+") s
--stringSum = sum $ map read $ words
stringSum s = sum $ map read $ map deletePlus (words s)

deletePlus :: String -> String
deletePlus (c:s) = if (c == '+') then (checkAfterPlus s) else c:s

checkAfterPlus :: String -> String
checkAfterPlus "" = error("bad format")
checkAfterPlus (c:s) = if (isDigit c) then (c:s) else error("bad format")