kolCifr :: Int -> Int

kolCifr a = if a < 1 then 0 else 1 + kolCifr(a `div` 10)

sumCifr :: Int -> Int

sumCifr a = if a < 1 then 0 else a `mod` 10 + sumCifr(a `div` 10)

kollats :: Int -> (Int, Int)
kollats n = kollats' n 0 0

kollats' ::  Int -> Int -> Int -> (Int, Int)
kollats' 1 acc_base acc_max = (acc_base, acc_max)
kollats' n acc_base acc_max
    |even n = kollats' (n `div` 2) (acc_base + 1) (max n acc_max)
    |otherwise = kollats' (n*3 + 1) (acc_base + 1) (max n acc_max)

chekStepen :: Int -> Bool
chekStepen n = if n `mod` 2 /= 0 then False else if n == 2 then True else chekStepen(n `div` 2)
