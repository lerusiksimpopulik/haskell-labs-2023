-- Task 1

quadraticSolver :: Double -> Double -> Double -> Maybe (Double, Double)
quadraticSolver a b c
    |    d < 0 = Nothing
    |    otherwise = Just ((-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a))
    where d = b * b - 4 * a * c

-- Task 2

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_:xs) = Just xs

maybeInit :: [a] -> Maybe [a]
maybeInit [] = Nothing
maybeInit xs = Just (init xs)

maybeFind :: (a -> Bool) -> [a] -> Maybe a
maybeFind _ [] = Nothing
maybeFind predicate (x:xs)
    |   predicate x = Just x
    |   otherwise   = maybeFind predicate xs

import Data.List (maximumBy)
import Data.Ord (comparing)

data DogBreed = GoldenRetrievers
              | BostonTerriers
              | LabradorRetrievers
              | Poodles
              | BorderCollie
              | Beagle
              | IrishSetter
              | Staffordshire
              | Bull
              | Terrier
    deriving (Show, Eq)

data Gender = Male | Female
    deriving (Show, Eq)

data Dog = Dog { name :: String
               , age :: Int
               , gender :: Gender
               , breed :: DogBreed
               , isGoodBoy :: Bool
               } deriving (Show, Eq)

dogs :: [Dog]
dogs = [ Dog "Leander" 12 Male Beagle False
       , Dog "Ouranos" 1 Male Poodles True
       , Dog "Pegasus" 2 Female Beagle False
       , Dog "Atlas" 8 Female GoldenRetrievers True
       , Dog "Castor" 6 Male LabradorRetrievers True
       , Dog "Apollo" 3 Female Beagle False
       , Dog "Narkissos" 15 Male Beagle True
       , Dog "Dardanos" 7 Female Terrier True
       , Dog "Ajax" 4 Male IrishSetter False
       , Dog "Pyrrhos" 2 Female BorderCollie False
       , Dog "Patroclus" 6 Male Bull True
       , Dog "Iacchus" 4 Female Beagle True ]

-- Dogs which are good boys
goodBoys :: [Dog]
goodBoys = filter isGoodBoy dogs

-- Dogs with a name longer than 7 symbols
longNamedDogs :: [Dog]
longNamedDogs = filter (\d -> length (name d) > 7) dogs

-- Among dogs, which is the most popular gender?
mostPopularDogGender :: Gender
mostPopularDogGender =
  let maleCount = length $ filter (\d -> gender d == Male) dogs
      femaleCount = length $ filter (\d -> gender d == Female) dogs
  in if maleCount > femaleCount then Male else Female


-- Oldest dog
oldestDog :: Dog
oldestDog = maximumBy (comparing age) dogs

-- Average dog age
averageDogAge :: Int
averageDogAge = if dogCount == 0 then 0 else totalAge `div` dogCount
    where
        totalAge = sum (map age dogs)
        dogCount = length dogs

-- Finds dogs with a given breed
dogsByBreed :: DogBreed -> [Dog]
dogsByBreed breedToFind = filter (\d -> breed d == breedToFind) dogs



-- Task 4.1

-- Complex numbers
data ComplexNumber = Complex { realPart :: Double, imaginaryPart :: Double }

addComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
addComplex (Complex a b) (Complex c d) = Complex (a + c) (b + d)

subtractComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
subtractComplex (Complex a b) (Complex c d) = Complex (a - c) (b - d)

multiplyComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
multiplyComplex (Complex a b) (Complex c d) =
    Complex (a * c - b * d) (a * d + b * c)

divideComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
divideComplex (Complex a b) (Complex c d) =
    let denominator = c^2 + d^2
    in Complex ((a * c + b * d) / denominator) ((b * c - a * d) / denominator)

conjugateComplex :: ComplexNumber -> ComplexNumber
conjugateComplex (Complex a b) = Complex a (-b)

absComplex :: ComplexNumber -> Double
absComplex (Complex a b) = sqrt (a^2 + b^2)

-- Singly Linked List
data MyList a = EmptyList | ListNode a (MyList a) deriving Show

fromList :: [a] -> MyList a
fromList = foldr ListNode EmptyList

toList :: MyList a -> [a]
toList EmptyList = []
toList (ListNode x rest) = x : toList rest

reverseMyList :: MyList a -> MyList a
reverseMyList lst = reverseAcc lst EmptyList
        where
    reverseAcc EmptyList acc = acc
    reverseAcc (ListNode x xs) acc = reverseAcc xs (ListNode x acc)

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList _ EmptyList = EmptyList
mapMyList f (ListNode x rest) = ListNode (f x) (mapMyList f rest)
