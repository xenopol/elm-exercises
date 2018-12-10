module Leap exposing (isLeapYear)


isDivisibleBy year number =
    remainderBy number year == 0


isLeapYear : Int -> Bool
isLeapYear year =
    isDivisibleBy year 4
        && not (isDivisibleBy year 100)
        || isDivisibleBy year 400
