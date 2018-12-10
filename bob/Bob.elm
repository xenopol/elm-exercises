module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        isUpper =
            String.all Char.isUpper remark

        isQuestion =
            String.endsWith "?" remark
    in
    if isUpper && isQuestion then
        "Calm down, I know what I'm doing!"

    else if isUpper then
        "Whoa, chill out!"

    else if isQuestion then
        "Sure."

    else if String.isEmpty remark then
        "Fine. Be that way!"

    else
        "Whatever."
