module Pangram exposing (isPangram)

import Set exposing (Set)


isPangram : String -> Bool
isPangram =
    let
        numberOfAlphabetLetters =
            26
    in
    String.toList
        >> Set.fromList
        >> Set.map Char.toLower
        >> Set.filter Char.isLower
        >> Set.size
        >> (==) numberOfAlphabetLetters
