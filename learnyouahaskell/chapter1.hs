{-
    some list methods:
        length
        take
        drop
        tail
        last
        head
        init
        maximum
        minimum
        sum
        product
        elem
        "luckyscript" !! 5
-}
fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' num = fibonacci' (num-1) + fibonacci' (num-2)