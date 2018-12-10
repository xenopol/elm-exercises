module Triangle exposing (Triangle(..), triangleKind)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    let
        areSidesBiggerThanZero =
            x > 0 && y > 0 && z > 0

        areSidesTheRightLength =
            x + y >= z && x + z >= y && y + z >= x
    in
    if not areSidesBiggerThanZero then
        Err "Invalid lengths"

    else if not areSidesTheRightLength then
        Err "Violates inequality"

    else if x == y && y == z then
        Ok Equilateral

    else if x == y || x == z || y == z then
        Ok Isosceles

    else
        Ok Scalene
