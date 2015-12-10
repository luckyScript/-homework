-- Week 4
-- Kay
-- 2015 Nov
type Expression = [Token]
data Token = Plus | Minus | Times | Divided_By | Power | Num {number :: Double}
    deriving (Show)
tokenizer :: String -> Expression
tokenizer expression_as_string = case expression_as_string of 
    "" -> []
    c: cs -> case c of 
        '+' -> Plus       : tokenizer cs
        '-' -> Minus      : tokenizer cs
        '*' -> Times      : tokenizer cs
        '/' -> Divided_By : tokenizer cs
        '^' -> Power : tokenizer cs
        _ | c `elem` ['0' .. '9'] -> case reads expression_as_string of
                      [(value, rest)] -> Num value : tokenizer rest
                      _               -> error "Could not read number"
          | c `elem` [' ', '\t'] -> tokenizer cs
          | otherwise -> error "Unknown Symbol"

expression_to_string :: Expression -> String
expression_to_string expr = case expr of 
    [] -> ""
    e: es -> case e of
        Plus        -> "+" ++ expression_to_string es
        Minus       -> "-" ++ expression_to_string es
        Times       -> "*" ++ expression_to_string es
        Divided_By  -> "/" ++ expression_to_string es
        Power       -> "^" ++ expression_to_string es
        Num x       -> (show x) ++ expression_to_string es

eval :: Expression -> Expression
eval exp = case exp of
    [] -> []
    Num x -> [Num x]
    Num x:op:Num y:[] -> case op of
        Plus                -> [Num (x + y)]
        Minus               -> [Num (x - y)]
        Times               -> [Num (x * y)]
        Divided_By          -> [Num (x / y)]
        Power               -> [Num (x ** y)]
        _                   -> error "~~~"
    Num x:op:Num y :xs -> case op of
        Plus                -> eval ((Num (x + y)):xs)
        Minus               -> eval ((Num (x - y)):xs)
        Times               -> eval ((Num (x * y)):xs)
        Divided_By          -> eval ((Num (x / y)):xs)
        Power               -> eval ((Num (x ** y)):xs)
        _                   -> error "~~~"