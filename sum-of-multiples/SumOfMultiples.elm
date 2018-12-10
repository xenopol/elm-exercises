module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples multiples limit =
    List.range 1 (limit - 1)
        |> List.filter (isDivisible multiples)
        |> List.sum


isDivisible : List Int -> Int -> Bool
isDivisible multiples number =
    List.any (\m -> remainderBy m number == 0) multiples
