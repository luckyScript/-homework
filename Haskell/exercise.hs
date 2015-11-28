applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs)
    |f x        = x:filter' f xs
    |otherwise  = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []    = []
quicksort (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs 

max' :: (Ord a) => [a] -> a
max' = foldl1 max

