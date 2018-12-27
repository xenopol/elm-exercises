module Say exposing (SayError(..), say)


type SayError
    = Negative
    | TooLarge


say : Int -> Result SayError String
say number =
    if number > 999999999999 then
        Err TooLarge

    else if number < 0 then
        Err Negative

    else
        buildNumber number |> Ok


buildNumber : Int -> String
buildNumber n =
    if n < 10 then
        sayUpTo9 n

    else if n < 20 then
        sayUpTo19 n

    else if n < 100 then
        sayUpTo99 n

    else if n < 1000 then
        sayUpTo n "hundred" 100 buildNumber

    else if n < 1000000 then
        sayUpTo n "thousand" 1000 buildNumber

    else if n < 1000000000 then
        sayUpTo n "million" 1000000 buildNumber

    else
        sayUpTo n "billion" 1000000000 buildNumber


sayUpTo9 : Int -> String
sayUpTo9 n =
    case n of
        0 ->
            "zero"

        1 ->
            "one"

        2 ->
            "two"

        3 ->
            "three"

        4 ->
            "four"

        5 ->
            "five"

        6 ->
            "six"

        7 ->
            "seven"

        8 ->
            "eight"

        9 ->
            "nine"

        _ ->
            ""


sayUpTo19 : Int -> String
sayUpTo19 n =
    if n < 10 then
        sayUpTo9 n

    else if n == 14 || n > 15 then
        sayUpTo9 (n - 10) ++ "teen"

    else
        case n of
            10 ->
                "ten"

            11 ->
                "eleven"

            12 ->
                "twelve"

            13 ->
                "thirteen"

            15 ->
                "fifteen"

            _ ->
                ""


sayTens : Int -> String
sayTens n =
    if n >= 90 then
        "ninety"

    else if n >= 80 then
        "eighty"

    else if n >= 70 then
        "seventy"

    else if n >= 60 then
        "sixty"

    else if n >= 50 then
        "fifty"

    else if n >= 40 then
        "forty"

    else if n >= 30 then
        "thirty"

    else if n >= 20 then
        "twenty"

    else
        ""


sayUpTo99 : Int -> String
sayUpTo99 n =
    let
        remainder =
            remainderBy 10 n
    in
    if n < 20 then
        sayUpTo19 n

    else if remainder > 0 then
        sayTens n ++ "-" ++ sayUpTo19 remainder

    else
        sayTens n


sayUpTo : Int -> String -> Int -> (Int -> String) -> String
sayUpTo n word factor func =
    let
        remainder =
            remainderBy factor n

        count =
            toFloat n / toFloat factor |> truncate >> func
    in
    if n < factor then
        func n

    else if remainder < 100 && remainder > 0 then
        count ++ " " ++ word ++ " and " ++ func remainder

    else if remainder > 0 then
        count ++ " " ++ word ++ " " ++ func remainder

    else
        count ++ " " ++ word
