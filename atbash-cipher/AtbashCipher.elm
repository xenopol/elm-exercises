module AtbashCipher exposing (decode, encode)


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyz"


encode : String -> String
encode plain =
    plain
        |> transposeLetter
        |> formatCipher


decode : String -> String
decode cipher =
    transposeLetter cipher


formatCipher : String -> String
formatCipher cipher =
    cipher
        |> String.toList
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, c ) acc ->
                if i > 0 && remainderBy 5 i == 0 then
                    acc ++ " " ++ String.fromChar c

                else
                    acc ++ String.fromChar c
            )
            ""


transposeLetter : String -> String
transposeLetter text =
    text
        |> String.filter Char.isAlphaNum
        |> String.map Char.toLower
        |> String.foldl
            (\c acc ->
                let
                    letter =
                        String.fromChar c

                    index =
                        String.indexes letter alphabet
                            |> List.head
                            |> Maybe.withDefault 0

                    alphabetLength =
                        String.length alphabet
                in
                if Char.isDigit c then
                    acc ++ String.fromChar c

                else if index == 0 then
                    acc ++ String.dropLeft (alphabetLength - 1) alphabet

                else if index == alphabetLength - 1 then
                    acc ++ String.dropRight (alphabetLength - 1) alphabet

                else
                    acc ++ String.slice -(index + 1) -index alphabet
            )
            ""
