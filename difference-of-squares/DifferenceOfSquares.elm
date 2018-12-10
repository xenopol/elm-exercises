module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)

import List exposing (map, range, sum)


squareOfSum : Int -> Int
squareOfSum n =
    range 1 n
        |> sum
        |> (\x -> x ^ 2)


sumOfSquares : Int -> Int
sumOfSquares n =
    range 1 n
        |> map (\x -> x ^ 2)
        |> sum


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
