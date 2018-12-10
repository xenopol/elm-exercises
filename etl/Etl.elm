module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform =
    Dict.toList
        >> List.concatMap convert
        >> Dict.fromList

convert : (Int, List String) -> List (String, Int)
convert ( key, values ) =
    List.map (\v -> ( String.toLower v, key )) values
