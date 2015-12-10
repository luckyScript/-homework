powerset :: [a] -> [[a]]
powerset list = case list of
    []   -> []:[]
    x:xs -> map (x:) (powerset xs) ++ powerset xs
