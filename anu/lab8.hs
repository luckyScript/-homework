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


-- exercise 3

type BS_Tree a = Binary_Tree a

is_vaild_binary_search_tree :: Ord a => BS_Tree a -> Bool   
is_vaild_binary_search_tree bs_tree = case bs_tree of 
    Null        -> True
    Node {element = x, left_tree = left, right_tree = right}
        | maximum' left < x && minimum' right > x         -> True
        | otherwise                                     -> False

maximum' :: (Ord a) => BS_Tree a -> a
maximum' bs_tree = case bs_tree of
    Null        -> error "maximum of empty list" 
    Node {element = x, left_tree = left, right_tree = right}
        | right == Null     -> x
        | otherwise         -> maximum' right

minimum' :: (Ord a) => BS_Tree a -> a
minimum' bs_tree = case bs_tree of
    Null        -> error "maximum of empty list" 
    Node {element = x, left_tree = left, right_tree = right}
        | left == Null     -> x
        | otherwise         -> minimum' left
contains_element :: Ord a => a -> BS_Tree a -> Bool
contains_element a bs_tree = case bs_tree of
    Null        -> False
    Node {element = x, left_tree = left, right_tree = right}
        | a == x    -> True
        | otherwise -> contains_element a left || contains_element a right

flatten_in_order :: Ord a => BS_Tree a -> [a]
flatten_in_order bs_tree = sort' (flatten_tree bs_tree)

sort' :: (Ord a) => [a] -> [a]
sort' []    = []
sort' (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger         = filter (> x) xs
    in sort' smallerOrEqual ++ [x] ++ sort' larger

insert_element_to_tree :: Ord a => a -> BS_Tree a -> BS_Tree a
insert_element_to_tree a bs_tree = case bs_tree of 
    Null    ->  Node {element = a, left_tree = Null, right_tree = Null}
    Node {element = x, left_tree = left, right_tree = right}
        | is_vaild_binary_search_tree bs_tree == False  -> error "not a vaild bs_tree"
        | x > a         -> insert_element_to_tree a left
        | otherwise     -> insert_element_to_tree a right

-- exercise 4

data Operand = Plus | Minus | Times | Divided_By | Power
    deriving (Show,Eq)

data Expression a = Number a | Node' (Expression a) Operand (Expression a)
    deriving (Show,Eq)


eval :: Floating a => Expression a -> Expression a
eval (Node' (l) op (r)) = case (l,r) of
    (Number x,Number y)
        | op == Plus           -> Number (x + y)
        | op == Minus          -> Number (x + y)
        | op == Times          -> Number (x + y)
        | op == Divided_By     -> Number (x + y)
        | op == Power          -> Number (x + y)
    (_,_) -> eval (Node' (eval l) op (eval r))
eval (Number a) = Number a


