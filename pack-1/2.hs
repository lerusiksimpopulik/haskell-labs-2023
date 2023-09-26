kolCifr :: Int -> Int

kolCifr a = if a < 1 then 0 else 1 + kolCifr(a `div` 10)

sumCifr :: Int -> Int

sumCifr a = if a < 1 then 0 else a `mod` 10 + sumCifr(a `div` 10)

chekStepen :: Int -> Bool
chekStepen n = if n `mod` 2 /= 0 then False else if n == 2 then True else chekStepen(n `div` 2)
