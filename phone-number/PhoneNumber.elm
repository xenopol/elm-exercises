module PhoneNumber exposing (getNumber)


getNumber : String -> Maybe String
getNumber phoneNumber =
    let
        number =
            String.filter Char.isDigit phoneNumber
        numberLength =
            String.length number
    in
    if
        numberLength == 11
            && String.startsWith "1" number
    then
        String.dropLeft 1 number |> Just

    else if numberLength == 10
        && String.left 1 number > "1"
        && (String.dropLeft 3 number |> String.left 1) > "1"
    then
        Just number

    else
        Nothing
