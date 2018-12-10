module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep predicate list =
    case list of
        [] ->
            []

        head :: rest ->
            if predicate head then
                head :: keep predicate rest

            else
                keep predicate rest


discard : (a -> Bool) -> List a -> List a
discard predicate list =
    keep (predicate >> not) list
