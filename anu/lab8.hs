-- name :Kay
-- Trees
-- 2015.12.14  

-- exercise 1
data Binary_Tree a = Null | Node {element::a,left_tree,right_tree::Binary_Tree a}
    deriving (Show,Eq)

max' :: (Integral b) => b -> b -> b
max' l r
    |(l > r)   = l
    |otherwise = r

size_of_tree :: (Integral b) => Binary_Tree a -> b
size_of_tree binary_tree = case binary_tree of
    Null        -> 0
    Node {element = _, left_tree = left, right_tree = right} -> size_of_tree left + size_of_tree right + 1

depth_of_tree :: (Integral b) => Binary_Tree a -> b
depth_of_tree binary_tree = case binary_tree of
    Null        -> 0
    Node {element = _, left_tree = left, right_tree = right} -> max' (depth_of_tree left) (depth_of_tree right) + 1

flatten_tree :: Binary_Tree a -> [a]
flatten_tree binary_tree = case binary_tree of
    Null -> []
    Node {element = x, left_tree = left, right_tree = right} -> x : (flatten_tree left ++ flatten_tree right)

leaves_of_tree ::(Eq a) => Binary_Tree a -> [a]
leaves_of_tree binary_tree = case binary_tree of 
    Null -> []
    Node {element = x, left_tree = left, right_tree = right}
        | left == Null && right == Null -> [x]
        | otherwise                     -> leaves_of_tree left ++ leaves_of_tree right


-- exercise 2
map_function_over_binary_tree :: (a -> b) -> Binary_Tree a -> Binary_Tree b
map_function_over_binary_tree f binary_tree = case binary_tree of
    Null -> Null
    Node {element = x, left_tree = left, right_tree = right} -> Node {element = (f x), left_tree = (map_function_over_binary_tree f left), right_tree = (map_function_over_binary_tree f right)} 


    