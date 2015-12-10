headPlusOne :: [Integer] -> Integer
headPlusOne list = case list of
    []  -> 0
    x:xs -> x+1

addTwohead :: [Integer] -> [Integer] -> Integer
addTwohead ls1 ls2 = case (ls1,ls2) of
    ([],[])     -> 0
    ([],y:ys)   -> y
    (x:xs,[])   -> x
    (x:xs,y:ys) -> x+y

firstDigit :: String -> Char
firstDigit st
    | st == []  = '\0'
    | 