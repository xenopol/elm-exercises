module AllYourBase exposing (rebase)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if
        List.sum digits == 0
            || List.any ((>) 0) digits
            || List.any ((<=) inBase) digits
            || List.any ((>) 2) [ inBase, outBase ]
    then
        Nothing

    else
        toBase10 inBase digits
            |> toOutBase outBase
            |> Just



-- converts a list of digits from any base to base 10
-- e.g.: toBase10 2 [1,0,1] -> 5, toBase10 7 [2,5,0] -> 133


toBase10 : Int -> List Int -> Int
toBase10 base digits =
    List.foldl (\digit acc -> (acc * base) + digit) 0 digits



-- converts a number from base 10 to any base as a list of digits
-- e.g.: toOutBase 2 5 -> [1,0,1], toOutBase 7 133 -> [2,5,0]


toOutBase : Int -> Int -> List Int
toOutBase base n =
    let
        convert base_ n_ =
            if n_ == 0 then
                []

            else
                remainderBy base_ n_ :: convert base_ (n_ // base_)
    in
    convert base n |> List.reverse
