# author kay
# Find the element that appears once

# Given an array where every element occurs three times, except one element which occurs only once. Find the element that occurs once. Expected time complexity is O(n) and O(1) extra space.
# Examples:

# Input: arr[] = {12, 1, 12, 3, 12, 1, 1, 2, 3, 3}
# Output: 2
def chooseOnce(arr):
    once, twice = 0, 0
        for x in arr:
            once, twice  = (once ^ x) & ~twice , (once & x) | (twice & ~x)
        print once
chooseOnce([12,1,12,3,12,1,1,2,3,3])