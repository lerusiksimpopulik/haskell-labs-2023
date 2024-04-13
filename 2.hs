import Control.Monad.State

type GreekData = [(String, [Integer])]
greekDataA :: GreekData
greekDataA = [ ("alpha", [5, 10])
             , ("beta", [0, 8])
             , ("gamma", [18, 47, 60])
             , ("delta", [42])
             ]

greekDataB :: GreekData
greekDataB = [ ("phi", [53, 13])
             , ("chi", [21, 8, 191])
             , ("psi", [])
             , ("omega", [6, 82, 144])
             ]

--myHeadMay :: [a] -> Maybe a
--myHeadMay = undefined

--myDivMay :: Double -> Double -> Maybe Double
--myDivMay = undefined

--maximumMay :: [a] -> Maybe a
--maximumMay = undefined

--tailMay :: [a] -> Maybe a
--tailMay = undefined

myHeadMay :: [a] -> Maybe a
myHeadMay []     = Nothing
myHeadMay (x:_)  = Just x

myDivMay :: Double -> Double -> Maybe Double 
myDivMay _ 0   = Nothing
myDivMay x y   = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []  = Nothing
maximumMay xs  = Just (maximum xs)

tailMay :: [a] -> Maybe [a]
tailMay []      = Nothing
tailMay (_:xs)  = Just xs

-- {-
--  tl;dr implement the function WITHOUT do-notation or any monad magic. only pattern-matching, where and let in

--  first query the GreekData that is passed in,
--  look up the string passed in the second argument,
--  and retrieve the corresponding list of Integers. Call this list xs.
--  Next calculate the maximum of the tail of xs
--  (Don’t use any pattern matching here.
--  Use case expressions and the maximumMay and tailMay functions)
--  Take the maximum and divide it by the head of the list (using myHeadMay and myDivMay functions).
--  If any of these operations along the way return Nothing, then your function should return Nothing.
--  But if everything succeeds, then return the final quotient.
--  One hint… you’ll need to use the fromIntegral function to convert your two Integers to Doubles for the final call to myDivMay.
-- -}
queryGreek :: GreekData -> String -> Maybe Double
queryGreek d s = case lookup s d of 
    Nothing -> Nothing
    Just xs -> case tailMay xs of 
        Nothing -> Nothing
        Just ts -> case myHeadMay xs of 
            Nothing -> Nothing
            Just headelem -> case maximumMay ts of
                Nothing -> Nothing
                Just maxelem -> case myDivMay (fromIntegral maxelem) (fromIntegral headelem) of
                    Nothing -> Nothing
                    Just ans -> Just ans
                                                 

-- -- queryGreek greekDataA "alpha" == Just 2.0

-- -- Now do the same whole thing, but using do-notation, since Maybe is a Monad
queryGreekPro :: GreekData -> String -> Maybe Double
queryGreekPro d s= do
    xs <- lookup s d
    ts <- tailMay xs
    headelem <- myHeadMay xs
    maxelem <- maximumMay ts
    ans <- myDivMay (fromIntegral maxelem) (fromIntegral headelem)
    return ans


-- state monad
--rollDice = undefined
type RandState = [Integer] 

rollDice :: State RandState Integer
rollDice = do
    s <- get
    case s of
        [] -> error "THE END"
        (x:xs) -> do
            put (xs++[x])
            return (head xs)

game :: State RandState String
game = do
    firstPlayerRes <- rollDice
    secondPlayerRes <- rollDice
    if firstPlayerRes >= secondPlayerRes then return "First wins" else return "Second wins"

runGame :: String
runGame = evalState game startSeed
    where startSeed = [4, 3, 5, 2, 1, 6]
