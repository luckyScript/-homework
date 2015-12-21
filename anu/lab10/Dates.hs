-- File/Module name: Dates (.hs)
-- Author: Kay
-- Date: 20151221
-- Description: Provides types,names and functions for dates.

module Dates

(
    Date (Date,day',month',year'),
    Day,Month,Year,
    Day_Names (Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday),
    Month_Names (January,February,March,April,May,June,July,August,September,October,November,December),
    iso_day_no_to_name, -- :: Posotive -> Day_Names
    day_name_to_iso_day_no -- :: Day_Names -> Positive
) where

import Integer_Subtypes

data Day_Names = 
    Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
        deriving (Eq,Enum,Show)

data Month_Names = 
    January | February | March | April | May | June | July | August |
    September | October | November | December
        deriving (Eq,Enum,Show)

type Day = Positive
type Month = Month_Names
type Year = Natural

data Date = Date {day' :: Day,month' :: Month,year' :: Year}
    deriving(Eq,Show)

iso_day_no_to_name :: Positive -> Day_Names
iso_day_no_to_name iso_day_no = toEnum (fromInteger (toInteger iso_day_no-1))

day_name_to_iso_day_no :: Day_Names -> Positive
day_name_to_iso_day_no day = fromInteger (1+toInteger(fromEnum day))

