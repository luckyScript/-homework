data Grades = Fail | Pass | Credit | Distinction | High_Distinction
    deriving (Show)
data Maybe a = Nothing | Just a
grade' :: Integer -> Main.Maybe Grades
grade' mark
    | mark >= 80 && mark <= 100 = Just High_Distinction
    | mark >= 70 && mark < 80   = Just Distinction
    | mark >= 60 && mark < 70   = Just Credit
    | mark >= 50 && mark < 60   = Just Pass
    | mark >=  0 && mark < 50   = Just Fail
    | mark <   0 || mark > 100  = Nothing
    | otherwise = error "Program error: Non-exhaustive guards in function: grade"


area_of_triangle' :: Float -> Float -> Float -> Main.Maybe Float
area_of_triangle' x y z 
    | (x + y > z)&&(x + z > y)&&(y + z > x) = Just sqrt(heron * (heron - x) * (heron - y) * (heron -z))
    | otherwise    = Nothing
    where heron = (x + y + z)/2

data Day_Names =
    Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq, Enum, Show)
day_name_to_iso_day_no :: Day_Names -> Integer 
day_name_to_iso_day_no day = case day of
    Monday    -> 1
    Tuesday   -> 2
    Wednesday -> 3
    Thursday  -> 4
    Friday    -> 5
    Saturday  -> 6
    Sunday    -> 7
day_name_to_iso_no_guard :: Day_Names -> Integer
day_name_to_iso_no_guard day
    | day == "Monday"       = 1
    | day == "Tuesday"      = 2

maybe_divide :: Maybe Integer -> Maybe Integer -> Maybe Integer
--maybe_divide Maybe_x Maybe_y = case (Maybe_x,Maybe_y) of
    (Nothing,_) -> Nothing
    (_,Nothing) -> Nothing
    (_,0) -> Nothing
    (Just x, Just y) -> x `div` y

data Move :: Paper | Rock | Scissors 
    deriving (Eq, Show) 
data Result = Win | Draw | Lose 
    deriving (Eq, Show)

-- function returns the move which beats the other move
beats :: Move -> Move -> Move 
beats move = case move of
        paper    -> Scissors
        rock     -> Paper
        scissors -> Rock
-- find out what the score is for moves and return Win or Draw or Lose

score :: Move -> Move -> Result
score this_move opposing_move = case this_move of
        opposing_move -> Draw
        _
               | this_move == beats opposing_move -> Win
               | _                               -> Lose