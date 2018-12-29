module Acronym exposing (abbreviate)


abbreviate : String -> String
abbreviate =
    String.words
        >> List.concatMap (String.split "-")
        >> List.map (String.left 1 >> String.toUpper)
        >> String.concat
