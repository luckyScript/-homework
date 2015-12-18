-- name: kay
-- higher order function

-- exercise 1
apply_function :: (Integer -> Integer) -> Integer -> Integer
apply_function func i = func i

double :: Integer -> Integer
double x = 2*x

triple :: Integer -> Integer
triple m = m+m+m

reverse_sign :: Integer -> Integer
reverse_sign m = -m

apply_function_over_list :: (Integer -> Integer) -> [Integer] -> [Integer]
apply_function_over_list func ls = case ls of
    []  -> []
    x:xs -> (func x) : (apply_function_over_list func xs)

select_where_true :: (Double -> Bool) -> [Double] -> [Double]
select_where_true func ls = case ls of
    [] -> []
    x:xs
        |func x -> x:select_where_true func xs
        |otherwise -> select_where_true func xs

is_negative :: Double -> Bool
is_negative x = x < 0.0

is_positive :: Double -> Bool
is_positive x = x > 0.0

-- Interlude : Polymorphism and Higher-Order Functions

general_length :: Integral b => [a] -> b
general_length ls = case ls of
    []      -> 0
    _:xs    -> 1 + general_length xs


-- exercise 2 Generalisation
apply_function_over_list' :: (a -> a) -> [a] -> [a]
apply_function_over_list' func ls = case ls of
    []      -> []
    x:xs    -> (func x) : (apply_function_over_list' func xs)

select_where_true' :: (a -> Bool) -> [a] -> [a]
select_where_true' func ls = case ls of
    []  -> []
    x:xs
        |func x -> x:select_where_true' func xs
        |otherwise -> select_where_true' func xs

combine_lists_with_binary_operation :: (a -> a -> a) -> [a] -> [a] -> [a]
combine_lists_with_binary_operation func ls1 ls2 = case (ls1,ls2) of
    ([],_)  -> []
    (_,[])  -> []
    (x:xs,y:ys) -> func x y : combine_lists_with_binary_operation func xs ys

combine_elements_into_tuples :: [a] -> [b] -> [(a,b)]
combine_elements_into_tuples ls1 ls2 = case (ls1,ls2) of
    ([],_)  -> []
    (_,[])  -> []
    (x:xs,y:ys) -> (x,y) : combine_elements_into_tuples xs ys

-- exercise 3 Put it into practice
convert_to_lower :: String -> String
convert_to_lower ls = foldr (\x acc -> if elem x ['A'..'Z'] then toEnum (fromEnum(x) + 32):acc else x:acc) [] ls 

remove_non_alphanum :: String -> String
remove_non_alphanum ls = foldr (\x acc -> if elem x (['a'..'z']++['A'..'Z']++['0'..'9']) then x:acc else acc) [] ls

dot_product :: (Num a) => [a] -> [a] -> [a]
dot_product ls1 ls2 = zipWith (*) ls1 ls2

is_Square :: Integral a => a -> Bool
is_Square i = floor (sqrt (fromIntegral i)) ^2 == i

list_sum :: Integral a => [a] -> a
list_sum ls = case ls of
    []  -> 0
    x:xs -> x + list_sum ls

sum_of_squares_up_to :: Integral a => [a] -> a
sum_of_squares_up_to ls = list_sum $ filter is_Square ls

--exercise 4
is_prime :: (Integral a, Eq a) => a -> Bool
is_prime a
   | a == 1 || a == 0 = False
   | otherwise = case (filter zero (prime_list a [2..a-1])) of
      [] -> True
      _-> False 
   where 
   zero :: Integral a => a -> Bool
   zero a = a == 0
   prime_list :: Integral a => a -> [a] -> [a]
   prime_list a list = case list  of
      x:xs -> mod a x : prime_list a xs  
      [] -> []    