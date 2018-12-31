module Isogram exposing (isIsogram)


isIsogram : String -> Bool
isIsogram sentence =
    let
        formatedSentence =
            sentence
                |> String.filter Char.isAlpha
                |> String.map Char.toLower
    in
    String.all (hasDuplicates formatedSentence) formatedSentence


hasDuplicates : String -> Char -> Bool
hasDuplicates string c =
    string
        |> String.indexes (String.fromChar c)
        |> List.length
        |> (==) 1
