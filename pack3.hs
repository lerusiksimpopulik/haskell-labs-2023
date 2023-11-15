fizzbuzz :: [String]
fizzbuzz = [if x `mod` 15 == 0 then "“fizzbuzz”" else if x `mod` 3 == 0 then "fizz" else if x `mod` 5 == 0 then "buzz" else show x | x <- [1..]]

dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]
dotsInCircle _ _ [] = []
dotsInCircle (x, y) r ((x1, y1):xs)
    | x - r <= x1 && x1 <= x + r && y - r <= y1 && y1 <= y + r = (x1, y1) : dotsInCircle (x, y) r xs
    | otherwise = dotsInCircle (x, y) r xs

dotsInCircle' :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]
dotsInCircle' (x,y) r arr = [(x1, y1) | (x1, y1) <- arr, x - r <= x1 && x1 <= x + r && y - r <= y1 && y1 <= y + r]

setAnd :: [Int] -> [Int] -> [Int]
setAnd xs ys = filter (\x -> x `elem` ys) xs

setAnd :: [Int] -> [Int] -> [Int]
setAnd xs ys = filter (\x -> x `elem` ys) xs

-- kolCifr 0 = 1
kolCifr x
    | x < 10 = 1
    | otherwise = 1 + kolCifr (x `div` 10)

sumCifr 0 = 0
-- sumCifr 1 = 1
sumCifr x = (x `mod` 10) + sumCifr (x `div` 10)

isPowerOfTwo :: Int -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo n
  | n > 1 && n `mod` 2 == 0 = isPowerOfTwo (n `div` 2)
  | otherwise = False

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

collatzSteps :: Int -> Int
collatzSteps n = length (collatz n)

collatzMaxValue :: Int -> Int
collatzMaxValue n = maximum (collatz n)

collatzFull :: Int -> (Int, Int)
collatzFull n = (collatzSteps n, collatzMaxValue n)
