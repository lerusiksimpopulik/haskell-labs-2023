import Data.Char

decToBin :: Int -> String
decToBin 0 = "0"
decToBin n = reverse (binStr n "")
  where binStr 0 acc = acc
        binStr x acc = binStr (x `div` 2) (show (x `mod` 2) ++ acc)

numberToList :: Int -> [Int]
numberToList 0 = [0]
numberToList n = reverse (helper n)
  where
    helper 0 = []
    helper x = x `mod` 10 : helper (x `div` 10)

gorner :: Int -> Int -> Int
gorner digits base = foldl (\acc digit -> acc * base + digit) 0 (numberToList digits)

findMissingNumber :: [Int] -> Int
findMissingNumber numbers = expectedSum - actualSum
  where
    n = length numbers + 1
    expectedSum = n * (n + 1) `div` 2
    actualSum = sum numbers

strToInt :: Char -> Int
strToInt x
    | y < 48 = error "not a number"
    | y > 57, y < 65 = error "not a number"
    | y <65 = y-48
    |otherwise = y -55
    where y = ord x

read' :: String -> Int
read' [] = error "don't do this"
read' (x:xs) = foldl (\acc x -> acc*10 + (strToInt x)) (strToInt x) xs

maybeBracket :: String -> Bool 
maybeBracket xs =  case xs of [] -> error "empty" 
                              xs -> checkBracket 0 xs 
 
proverka :: Int -> Bool
proverka a | a >= 0 = True
           | otherwise = False

checkBracket :: Int -> String -> Bool 
checkBracket acc xs| acc < 0 = False
                   | xs==[] =  proverka acc
                   | head xs == '(' = checkBracket (acc +1) (tail xs) 
                   | head xs == ')' = checkBracket (acc -1) (tail xs)
