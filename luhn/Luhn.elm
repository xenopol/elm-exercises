module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        inputWithoutSpaces =
            String.replace " " "" input
    in
    if
        String.length inputWithoutSpaces < 2
            || (String.all Char.isDigit inputWithoutSpaces |> not)
    then
        False

    else
        inputWithoutSpaces
            |> String.toList
            |> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)
            |> List.reverse
            |> List.indexedMap doubleEvenDigit
            |> List.sum
            |> remainderBy 10
            |> (==) 0


doubleEvenDigit : Int -> Int -> Int
doubleEvenDigit i d =
    let
        double =
            d * 2
    in
    if remainderBy 2 i == 0 then
        d

    else if double > 9 then
        double - 9

    else
        double
