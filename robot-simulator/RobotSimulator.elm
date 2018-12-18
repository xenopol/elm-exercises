module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    Robot North { x = 0, y = 0 }


turnRight : Robot -> Robot
turnRight robot =
    case robot.bearing of
        North ->
            { robot | bearing = East }

        East ->
            { robot | bearing = South }

        South ->
            { robot | bearing = West }

        West ->
            { robot | bearing = North }


turnLeft : Robot -> Robot
turnLeft robot =
    case robot.bearing of
        North ->
            { robot | bearing = West }

        East ->
            { robot | bearing = North }

        South ->
            { robot | bearing = East }

        West ->
            { robot | bearing = South }


advance : Robot -> Robot
advance robot =
    case robot.bearing of
        North ->
            { robot | coordinates = { x = robot.coordinates.x, y = robot.coordinates.y + 1 } }

        East ->
            { robot | coordinates = { x = robot.coordinates.x + 1, y = robot.coordinates.y } }

        South ->
            { robot | coordinates = { x = robot.coordinates.x, y = robot.coordinates.y - 1 } }

        West ->
            { robot | coordinates = { x = robot.coordinates.x - 1, y = robot.coordinates.y } }


simulate : String -> Robot -> Robot
simulate directions robot =
    String.foldl
        (\dir acc ->
            case Char.toLower dir of
                'a' ->
                    advance acc

                'l' ->
                    turnLeft acc

                'r' ->
                    turnRight acc

                _ ->
                    acc
        )
        robot
        directions
