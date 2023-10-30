fizzbuzz :: [String]
fizzbuzz = [if x `mod` 15 == 0 then "“fizzbuzz”" else if x `mod` 3 == 0 then "fizz" else if x `mod` 5 == 0 then "buzz" else show x | x <- [1..]]

dotsInCircle :: (Double, Double) -> Double -> [(Double, Double)] -> [(Double, Double)]
dotsInCircle _ _ [] = []
dotsInCircle (x, y) r ((x1, y1):xs)
    | x - r <= x1 && x1 <= x + r && y - r <= y1 && y1 <= y + r = (x1, y1) : dotsInCircle (x, y) r xs
    | otherwise = dotsInCircle (x, y) r xs

