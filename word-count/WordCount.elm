module WordCount exposing (wordCount)

import Dict exposing (Dict)


wordCount : String -> Dict String Int
wordCount =
    String.toLower
        >> String.words
        >> List.map (String.filter Char.isAlphaNum)
        >> List.filter (String.isEmpty >> not)
        >> List.foldl
            (\word acc ->
                Dict.update word (\field -> Maybe.withDefault 0 field + 1 |> Just) acc
            )
            Dict.empty
