module SceneObjectFactory exposing (..)

import Random exposing (Seed, generate, step)
import Utils exposing (probability)
import Vector2 exposing (Vector, negate, (<+>), scaleBy)
import List exposing (map2, length)
import Model exposing (..)
import Body exposing (init)


---------CREATE --------------


randomBody : BodyId -> Position -> Seed -> ( Body, Seed )
randomBody id pos seed =
    let
        ( angle, ns0 ) =
            step Utils.probability seed

        ( vel, ns1 ) =
            Utils.rndVector ns0

        ( size, ns2 ) =
            Utils.rndVector ns1

        effSize =
            scaleBy 50 (Vector (0.2 + 0.8 * abs size.x) (0.2 + 0.8 * abs size.y))

        len =
            Vector2.length effSize

        shape =
            Model.Circle (len / 2)

        newBody =
            init id shape pos (scaleBy 0.5 vel) 0
    in
        ( newBody, ns2 )


randomWalls : Int -> Vector -> Seed -> ( List LineSegment, Seed )
randomWalls howMany ext seed =
    let
        ( poss, seed0 ) =
            Utils.vectorList ext howMany seed

        ( dirAs, seed1 ) =
            Utils.vectorList (Vector 150 150) howMany seed0
    in
        ( map2 (\pos a -> ( pos, a )) poss dirAs, seed1 )
