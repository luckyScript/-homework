cartesian :: [a] -> [b] ->[(a,b)]
cartesian xs ys = [(x,y) | x <- xs,y <- ys]