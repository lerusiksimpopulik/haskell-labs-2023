head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

reverse' :: [Int] -> [Int]
reverse' [x] = [x] --base
reverse' (x:xs) = (reverse' xs) ++ [x]

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs

sum' :: Num a => [a] -> a --ограничитель класса
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a 
product' [] = 1
product' (x:xs) = x * product' xs

elem' :: Eq a => a -> [a] -> Bool --Класс Eq используется для типов, которые поддерживают проверку равенства
elem' _ [] = False
elem' y (x:xs) | y == x = True
               | otherwise = elem' y xs

-- У функции elem тип (Eq a) => a –> [a] –> Bool, потому что она при-
-- меняет оператор == к элементам списка, чтобы проверить, есть ли
-- в этом списке значение, которое мы ищем.
