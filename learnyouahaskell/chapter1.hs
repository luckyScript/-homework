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
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname