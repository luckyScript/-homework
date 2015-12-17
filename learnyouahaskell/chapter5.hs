{-
    foldl (\acc x -> acc + x) 0 xs
    note: lambda para is different from foldr
-}
{-
    foldr (\x acc -> acc + x) 0 xs
    foldr can handle infinity list
-}

{-
    function composition: (f.g)(x) = f(g(x))
    (.) :: (b -> c) -> (a -> b) -> a -> c
    f.g = \x -> f $ g x
-}