reverse_of :: [a] -> [a]
reverse_of list = case list of
   []    ->  []
   c: cs -> concatenate (reverse_of cs) [c]
   where
      concatenate :: [a] -> [a] -> [a]
      concatenate s1 s2 = case s2 of
         []    -> s1
         c: cs -> c : concatenate cs s1

is_palindromic :: (Eq a) => [a] -> Bool
is_palindromic list = (list == reverse_of list)

equal_list :: [a] -> [a] -> Bool
equal_list list_1 list_2 = case (list_1, list_2) of
      ([], [])       -> False
      ([], _ )       -> False
      (_ , [])       -> False
      (x: xs, y: ys) -> (x == y) && (xs `equal_list` ys)
