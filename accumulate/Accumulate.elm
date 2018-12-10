module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate func input =
    case input of
        h :: rest ->
            func h :: accumulate func rest

        [] ->
            []
