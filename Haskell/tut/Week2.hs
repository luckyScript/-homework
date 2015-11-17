-- Week 2 Practical
-- Kay, Nov 2015.
cube :: Integer -> Integer
cube x = x * x * x

edge_length :: Integer
edge_length = 3

volume :: Integer
volume = cube edge_length

surface_area_with_radius :: Float -> Float
surface_area_with_radius r = 4.0 * pi * r * r

--------------------------------------------------
area_of_triangle :: Float -> Float -> Float -> Float
area_of_triangle x y z = sqrt(heron * (heron - x) * (heron - y) * (heron -z))
    where heron = (x + y + z)/2

--------------------------------------------------
data Grades = Fail | Pass | Credit | Distinction | High_Distinction
    deriving (Show)
grade :: Integer -> Grades
grade mark
    | mark >= 80 && mark <= 100 = High_Distinction
    | mark >= 70 && mark < 80   = Distinction
    | mark >= 60 && mark < 70   = Credit
    | mark >= 50 && mark < 60   = Pass
    | mark >=  0 && mark < 50   = Fail
    | mark < 0 || mark > 100    = error "Program error: Not a valid mark"
    | otherwise                 = error "Program error: Non-exhaustive guards in function: grade"

---------------------------------------------------
data Quadrants = Origin | Quadrant_I | Quadrant_II | Quadrant_III | Quadrant_IV | X_Axis_Positive | X_Axis_Negative | Y_Axis_Positive | Y_Axis_Negative | Origin_Point
    deriving (Show, Eq)
quadrant :: Float -> Float -> Quadrants
quadrant x y
    | x > 0 && y > 0        = Quadrant_I
    | x > 0 && y < 0        = Quadrant_IV
    | x > 0 && y == 0       = X_Axis_Positive
    | x < 0 && y > 0        = Quadrant_II
    | x < 0 && y < 0        = Quadrant_III
    | x < 0 && y == 0       = X_Axis_Negative
    | x == 0 && y > 0       = Y_Axis_Positive
    | x == 0 && y < 0       = Y_Axis_Negative
    | x == 0 && y == 0      = Origin_Point
    | otherwise             = error "Program error : Not effictive Parameter"

boom :: Integer -> Integer -> Integer
boom a b
    | a <= 1 || b <= 1 = a
    | otherwise        = a * boom (boom (a - 1) b) (b - 1)