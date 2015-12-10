difference :: (Eq a) => [a] -> [a] -> [a]
difference ls1 ls2 = case ls1 of
    []  -> []
    x:xs
        | elem x ls2      -> difference xs ls2
        | otherwise     -> x:difference xs ls2
