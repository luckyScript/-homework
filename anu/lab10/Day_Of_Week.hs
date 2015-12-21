-- File name: Day_Of_Week.hs
-- Author: Kay
-- Date: 20151221
-- Description: Provides functions to assist in calculationg the day of the week

import Dates
import Integer_Subtypes

days_since_1_January_0 :: Date -> Natural
days_since_1_January_0 (Date day month year)
    = (previous_days_this_month day)
    + (days_in_previous_months month year)
    + (days_before_this_year year)

previous_days_this_month :: Day -> Natural
previous_days_this_month day = from_Positive_to_Natural day

days_in_previous_months :: Month -> Year -> Natural
days_in_previous_months month year = 
    sum (take (fromEnum month) (month_lengths year))
    where
        month_lengths :: Year -> [Natural]
        month_lengths year = 
            [31,feb,31,30,31,30,31,31,30,31,30,31]
            where
                feb | is_leap_year year = 29
                    | otherwise         = 28

is_leap_year :: Year -> Bool
is_leap_year year 
    | year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0) = True
    | otherwise = False

days_before_this_year :: Year -> Natural
days_before_this_year year = (year - (year `quot` 4 - year `quot` 100 + year `quot` 400)) * 365 + (year `quot` 4 - year `quot` 100 + year `quot` 400)*366
