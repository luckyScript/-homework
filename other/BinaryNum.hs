binaryNum :: [Int] -> Int
binaryNum list = case list of
    []      -> 0
    x:xs
        | x == 0|| x == 1   -> x + 2 * binaryNum xs
        | otherwise         -> error "Program error: Not a valid binary"

