module Gigasecond exposing (add)

import Time exposing (Posix, millisToPosix, posixToMillis)


add : Posix -> Posix
add =
    posixToMillis >> (+) (10 ^ 12) >> millisToPosix
