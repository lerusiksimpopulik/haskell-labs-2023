kolCifr :: Int -> Int

kolCifr a = if a < 1 then 0 else 1 + kolCifr(a `div` 10)

sumCifr :: Int -> Int

sumCifr a = if a < 1 then 0 else a `mod` 10 + sumCifr(a `div` 10)

collatz  :: Int -> (Int, Int)
collatz n = collatz' n 0 0

collatz' ::  Int -> Int -> Int -> (Int, Int)
collatz' 1 acc_base acc_max = (acc_base, acc_max)
collatz' n acc_base acc_max
    |even n = collatz' (n `div` 2) (acc_base + 1) (max n acc_max)
    |otherwise = collatz' (n*3 + 1) (acc_base + 1) (max n acc_max)

chekStepen :: Int -> Bool
chekStepen n = if n `mod` 2 /= 0 then False else if n == 2 then True else chekStepen(n `div` 2)
