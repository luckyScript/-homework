difference :: (Eq a) => [a] -> [a] -> [a]
difference ls1 ls2 = foldr (\x ls -> if elem x ls2 then ls else x:ls) [] ls1
