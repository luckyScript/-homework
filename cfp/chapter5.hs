-- Haskell the craft of functional programming third edition
-- chapter 5
-- Kay, Nov, 2015
-- tuples and lists

maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs x y
    | y > x         = (y,1)
    | y < x         = (x,1)
    | otherwise     = (x,2)

-------------------------------------------------

{-data People = Person Name Age
            deriving (Eq,Show)
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n-}

--------------------------------------------------

type People = (Name,Age)
type Name = String
type Age = Int

showPerson :: People -> String
showPerson (st,n) = st ++ " -- " ++ show n

--------------------------------------------------
isEven :: Integer -> Bool
isEven x
    | x `mod` 2 == 0      = True
    | otherwise     = False
ex = [2,4,7]

--------------------------------------------------

addPairs :: [(Integer,Integer)] -> [Integer]
addPairs xs = [m+n | (m,n) <- xs]
--------------------------------------------------

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2*n | n <- xs]

--------------------------------------------------

capitalize :: String -> String
capitalize xs = [if fromEnum(n)>96&&fromEnum(n)<123 then toEnum(fromEnum (n) - 32) else n | n <- xs]


capitalizeLetters :: String -> String
capitalizeLetters xs = [toEnum(fromEnum (n) - 32) | n <- xs,fromEnum(n)>96&&fromEnum(n)<123]
--------------------------------------------------

divisor :: Integer -> [Integer]
divisor x = [n | n <- xs ,x `mod` n == 0]
    where xs = [1..x]

isPrime :: Integer -> Bool
isPrime x
    | divisor x == [1,x]        = True
    | otherwise                 = False
--------------------------------------------------

matches :: Integer -> [Integer] -> [Integer]
matches x xs = [n | n <- xs,n == x]

elem :: Integer -> [Integer] -> Bool
elem x xs
    | matches x xs == []        = False
    | otherwise                 = True

--------------------------------------------------

