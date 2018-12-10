module CollatzConjecture exposing (collatz)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive numbers are allowed"

    else if start == 1 then
        Ok 0

    else
        (if isEven start then
            start // 2

         else
            3 * start + 1
        )
            |> collatz
            |> Result.map ((+) 1)


isEven : Int -> Bool
isEven number =
    remainderBy 2 number == 0
