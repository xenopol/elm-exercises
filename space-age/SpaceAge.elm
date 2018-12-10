module SpaceAge exposing (Planet(..), ageOn)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


getYear : Float -> Float -> Float
getYear totalSeconds orbitalPeriod =
    let
        second =
            60

        hour =
            60

        day =
            24

        year =
            365.32
    in
    totalSeconds / second / hour / day / (year * orbitalPeriod)


ageOn : Planet -> Float -> Float
ageOn planet seconds =
    case planet of
        Mercury ->
            getYear seconds 0.2408467

        Venus ->
            getYear seconds 0.61519726

        Earth ->
            getYear seconds 1

        Mars ->
            getYear seconds 1.8808158

        Jupiter ->
            getYear seconds 11.862615

        Saturn ->
            getYear seconds 29.447498

        Uranus ->
            getYear seconds 84.016846

        Neptune ->
            getYear seconds 164.79132
