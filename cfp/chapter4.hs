-- Haskell the craft of functional programming third edition
-- chapter 4
-- Kay, Nov, 2015

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = (x `max` y) `max` z

-------------------------------------------------

between :: Integer -> Integer -> Integer -> Bool
between x y z
    | (x <= y && z >= y)||(x >= y && z <= y) = True
    | otherwise                              = False

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
    | between x y z     = y
    | between y x z     = x
    | otherwise         = z

-------------------------------------------------
maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour x y z w = ((x `max` y) `max` z) `max` w

-------------------------------------------------

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder x y z 
    | x <= y && y <= z      = True
    | otherwise             = False

-------------------------------------------------

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
    | x == y && x == z              = 3
    | x /= y && x /= z && z /= y    = 0
    | otherwise                     = 2

-------------------------------------------------

howManyFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyFourEqual x y z w
    | flag == 0                     = 0
    | flag == 4 || flag == 8        = 2
    | flag == 9                     = 3
    | flag == 12                    = 4
    | otherwise                     = error "Program error: Not Effective parameter"
    where flag = (howManyEqual x y z + howManyEqual x y w + howManyEqual x z w + howManyEqual y z w)

-------------------------------------------------

triArea :: Float -> Float -> Float -> Float
triArea a b c
    | possible      = sqrt(s * (s - a) * (s - b) * (s - c))
    | otherwise     = 0
    where
        s = (a + b + c)/2
        possible = (a + b) > c && (a + c) > b && (b + c) > a


-------------------------------------------------

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Int)
maxThreeOccurs x y z
    | maxNum == middleNum && maxNum == minNum       = (maxNum,3)
    | maxNum == middleNum && maxNum /= minNum       = (maxNum,2)
    | otherwise                         = (maxNum,1)

    where 
        maxNum = maxThree x y z
        middleNum = middleNumber x y z
        minThree :: Integer -> Integer -> Integer -> Integer
        minThree x y z = (x `min` y) `min` z
        minNum = minThree x y z

data Result = Rock | Paper | Scissors
win :: Result -> Result
win Rock = Sissors
win Sissors = Paper
win Paper = Rock






