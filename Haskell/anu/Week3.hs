data Grades = Fail | Pass | Credit | Distinction | High_Distinction
    deriving (Show)
data MyMaybe a = MyNothing | MyJust a
    deriving (Show)
grade' :: Integer -> MyMaybe Grades
grade' mark
    | mark >= 80 && mark <= 100 = MyJust High_Distinction
    | mark >= 70 && mark < 80   = MyJust Distinction
    | mark >= 60 && mark < 70   = MyJust Credit
    | mark >= 50 && mark < 60   = MyJust Pass
    | mark >=  0 && mark < 50   = MyJust Fail
    | mark <   0 || mark > 100  = MyNothing
    | otherwise = error "Program error: Non-exhaustive guards in function: grade"




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
    | day == Monday       = 1
    | day == Tuesday      = 2


mymaybe_divide :: MyMaybe Integer -> MyMaybe Integer -> MyMaybe Integer
mymaybe_divide myMaybe_x myMaybe_y = case (myMaybe_x,myMaybe_y) of
    (MyNothing,_) -> MyNothing
    (_,MyNothing) -> MyNothing
    (_,MyJust 0) -> MyNothing
    (MyJust x, MyJust y) -> MyJust (x `div` y)

data Move = Paper | Rock | Scissors 
    deriving (Eq, Show) 
data Result = Win | Draw | Lose 
    deriving (Eq, Show)

-- function returns the move which beats the other move
beats :: Move -> Move 
beats move = case move of
        Paper    -> Scissors
        Rock     -> Paper
        Scissors -> Rock
-- find out what the score is for moves and return Win or Draw or Lose
score :: Move -> Move -> Result
score this_move opposing_move = case this_move of
    opposing_move -> Draw   
    _
        | this_move == beats opposing_move -> Win
        | otherwise                        -> Lose
