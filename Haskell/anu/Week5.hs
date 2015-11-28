-- week 5
-- Kay
-- 2015 Nov
-- Recursion
sum' :: [Integer] -> Integer
sum' list = case list of
    []  -> 0
    x:xs -> x + sum' xs

product' :: [Num a] => [a] -> a
product' list = case list of
    []  -> 0
    x:xs -> x * product' xs

convert_to_upper_case :: String -> String
convert_to_upper_case list = case list of
    []      -> []
    x:xs    -> upper' x:convert_to_upper_case xs
        where upper' = fromEnum(toEnum x -32)

invert :: [a] -> [Integer]
invert xs = flodl : [] xs