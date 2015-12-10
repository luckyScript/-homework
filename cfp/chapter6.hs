-- Haskell the craft of functional programming third edition
-- chapter 6
-- Kay, Nov, 2015
-- programming with lists
superimposeChar :: Char -> Char -> Char
superimposeChar ch1 ch2 = case (ch1,ch2) of
    ('.','.') -> '.'
    _         -> '#'

superimposeLine :: [Char] -> [Char] ->[Char]
superimposeLine ls1 ls2 = case (ls1,ls2) of
    ([],[]) -> []
    (x:xs,y:ys) -> (superimposeChar x y) : (superimposeLine xs ys)

