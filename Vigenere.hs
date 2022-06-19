import Data.Char

--get only capital letters
--toUpper :: Char -> Char
inAlphabet :: String -> String
inAlphabet [] = []
inAlphabet (x:xs) = if ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z') then [toUpper x] ++ inAlphabet xs else inAlphabet xs

--get key or input String in numbers
stringToNum :: String -> [Int]
stringToNum [] = []
stringToNum (x:xs) = [ord x - 65] ++ stringToNum xs

--zip key list and string list together into pair
pairNums :: String -> String -> [ (Int,Int) ]
pairNums xs key = zip (stringToNum (inAlphabet xs)) (concat (repeat (stringToNum (inAlphabet key))))

--add tuple ints from pairStrings
addPairs :: [ (Int,Int) ] -> [Int]
addPairs [] = []
addPairs ((x,y):xs) = [(x + y) `mod` 26] ++ addPairs xs

--Convert array of int to String
intsToString :: [Int] -> String
intsToString [] = []
intsToString (x:xs) = [chr (x + 65)] ++ intsToString xs

--remove key
subtractPairs :: [ (Int,Int) ] -> [Int]
subtractPairs [] = []
subtractPairs ((x,y):xs) = if (x-y) > 0 then [(x - y) `mod` 26] ++ subtractPairs xs else [(x - y + 26) `mod` 26] ++ subtractPairs xs

encrypt :: String -> String -> String
encrypt key xs 
  | (key == "") = ""
  | otherwise = intsToString (addPairs (pairNums xs key))

decrypt :: String -> String -> String
decrypt key xs = intsToString (subtractPairs (pairNums xs key))

vigenere :: String -> (String -> String, String -> String)
vigenere key = (encrypt key, decrypt key)